#include "../../llvm-project/llvm/include/llvm/ADT/APFloat.h"
#include "../../llvm-project/llvm/include/llvm/ADT/STLExtras.h"
#include "../../llvm-project/llvm/include/llvm/IR/BasicBlock.h"
#include "../../llvm-project/llvm/include/llvm/IR/Constants.h"
#include "../../llvm-project/llvm/include/llvm/IR/DerivedTypes.h"
#include "../../llvm-project/llvm/include/llvm/IR/Function.h"
#include "../../llvm-project/llvm/include/llvm/IR/IRBuilder.h"
#include "../../llvm-project/llvm/include/llvm/IR/LLVMContext.h"
#include "../../llvm-project/llvm/include/llvm/IR/Module.h"
#include "../../llvm-project/llvm/include/llvm/IR/Type.h"
#include "../../llvm-project/llvm/include/llvm/IR/Verifier.h"
#include<cctype>
#include<cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>
using namespace llvm;
//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.


enum Token{
    tok_eof=-1,
    tok_def=-2,
    tok_extern=-3,
    tok_identifier=-4,
    tok_number=-5
};

static std::string IdentifierStr;
static double NumVal;

static int gettok(){
    static int LastChar=' ';
    while(isspace(LastChar)){
        LastChar=getchar();
    }

    if(isalpha(LastChar)){
        IdentifierStr = LastChar;
        while(isalnum(LastChar=getchar()))
            IdentifierStr+=LastChar;
        if(IdentifierStr=="def")
            return tok_def;
        if(IdentifierStr=="extern")
            return tok_extern;
        return tok_identifier;        
    }
    if(isdigit(LastChar)||LastChar=='.'){
        std::string Numstr;
        do
        {
            Numstr+= LastChar;
            LastChar = getchar();
        }while(isdigit(LastChar)||LastChar=='.');
        NumVal = strtod(Numstr.c_str(),nullptr);
        return tok_number;
    }
    if(LastChar=='#'){
        do
            LastChar=getchar();
        while(LastChar != EOF && LastChar != '\n' && LastChar != '\r');
        if(LastChar!=EOF)
            return gettok();
    }
    if(LastChar==EOF)
            return tok_eof;
    int ThisChar=LastChar;
    LastChar=getchar();
    return ThisChar;
}

//===------z----------------------------------------------------------------===//
// AST
//===----------------------------------------------------------------------===//

// This will create the AST


namespace{
    class ExprAST{
        public:
        virtual ~ExprAST()=default;
        virtual llvm::Value *codegen()=0;
    };
    class NumberExprAst : public ExprAST{
        double val;
        public:
            NumberExprAst(double val):val(val){}
            virtual llvm::Value *codegen() override;
            
    };
    
    class VariableExprAst : public ExprAST{
        std::string name;
        public:
            VariableExprAst(std::string name):name(name){}
            virtual llvm::Value *codegen() override;
    };
    
    class BinaryExprAst : public ExprAST{
        char op;
        std::unique_ptr<ExprAST> LHS,RHS;
        public:
            BinaryExprAst(char op,std::unique_ptr<ExprAST> LHS,std::unique_ptr<ExprAST> RHS):op(op),LHS(std::move(LHS)),RHS(std::move(RHS)){}
            virtual llvm::Value *codegen() override;
    };
     
    class CallExprAST:public ExprAST{
        std::string callee;
        std::vector<std::unique_ptr<ExprAST>> args;
        public:
            CallExprAST(const std::string callee,std::vector<std::unique_ptr<ExprAST>> args):callee(callee),args(move(args)){}
            virtual llvm::Value *codegen() override;
    };
    class PrototypeAST{
        std::string Name;
        std::vector<std::string> Args;
        public:
            PrototypeAST(const std::string &Name,std::vector<std::string> Args):Name(Name),Args(std::move(Args)){}
        const std::string &getName() const { return Name;}
        llvm::Function* codegen();
    };
    
    class FunctionAST
    {
        std::unique_ptr<PrototypeAST> proto;
        std::unique_ptr<ExprAST> body;
        public: 
            FunctionAST(std::unique_ptr<PrototypeAST> proto,std::unique_ptr<ExprAST> body)
            :proto(std::move(proto)),body(std::move(body)){}
            llvm::Function* codegen();
    };
    
   
}

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.


static int CurTok ;
static int getNextToken(){return CurTok = gettok();}

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<char, int> BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.

static int GetTokPrecedence(){
    if(!isascii(CurTok))
        return -1;
    int TokPrec = BinopPrecedence[CurTok];
    if(TokPrec<=0)
        return -1;
    return TokPrec;
}

std::unique_ptr<ExprAST> LogError(const char *Str){
    fprintf(stderr,"Error: %s\n",Str);
    return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str){
    LogError(Str);
    return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();


static std::unique_ptr<ExprAST> ParseNumberExpr(){
    auto Result = std::make_unique<NumberExprAst>(NumVal);
    getNextToken();
    return std::move(Result); 
}

static std::unique_ptr<ExprAST> ParseParenExpr(){
    getNextToken();
    auto V=ParseExpression();
    if(!V)
        return nullptr;
    if(CurTok!=')')
        return LogError("expected ')'");
    getNextToken();
    return V;
}

static std::unique_ptr<ExprAST> ParseIdentifierExpr(){
    std::string IdName=IdentifierStr;
    getNextToken();
    if (CurTok != '(') // Simple variable ref.
      return std::make_unique<VariableExprAst>(IdName);
    getNextToken();
    std::vector<std::unique_ptr<ExprAST>> Args;
    
    if(CurTok != ')'){
        while(true){
            
            if(auto Arg = ParseExpression())
                Args.push_back(std::move(Arg));
            else
                return nullptr;
            if(CurTok == ')')
                break;
            if(CurTok != ',')
                return LogError("Expected ')' or ',' in argument list");
            getNextToken();
        }
    }
    getNextToken();
    return std::make_unique<CallExprAST>(IdName,std::move(Args));
}
static std::unique_ptr<ExprAST> ParsePrimary(){
    switch(CurTok){
        default:
            return LogError("unknown token when expecting an expression");
        case tok_identifier:
            return ParseIdentifierExpr();
        case tok_number:
            return ParseNumberExpr();
        case '(':
            return ParseParenExpr();
    }
}

static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,std::unique_ptr<ExprAST> LHS){
    while(true){
        int TokPrec = GetTokPrecedence();
        if(TokPrec < ExprPrec)
            return LHS;
        int BinOp = CurTok;
        getNextToken();
        auto RHS= ParsePrimary();
        if(!RHS)
            return nullptr;
        int NextPrec = GetTokPrecedence();
        if(TokPrec < NextPrec){
            RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
            if (!RHS)
                return nullptr;
        }
        LHS = std::make_unique<BinaryExprAst>(BinOp, std::move(LHS), std::move(RHS));
    }
}
static std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParsePrimary();
  if (!LHS)
    return nullptr;

  return ParseBinOpRHS(0, std::move(LHS));
}

static std::unique_ptr<PrototypeAST> ParsePrototype(){
  if(CurTok!=tok_identifier)
    return LogErrorP("Expected function name in prototype");
  std::string FnName = IdentifierStr;
  getNextToken();
  if(CurTok != '(')
    return LogErrorP("Expected '(' in prototype");
  std::vector<std::string> ArgNames;
  while(getNextToken()==tok_identifier)
      ArgNames.push_back(IdentifierStr);
  if (CurTok != ')')
    return LogErrorP("Expected ')' in prototype");
  getNextToken();
  return std::make_unique<PrototypeAST>(FnName,ArgNames);
}

static std::unique_ptr<FunctionAST> ParseDefinition(){
  getNextToken();
  auto Proto = ParsePrototype();
  if(!Proto)
    return nullptr;
  if(auto E = ParseExpression())
    return std::make_unique<FunctionAST>(std::move(Proto),std::move(E));
  return nullptr;
}

static std::unique_ptr<PrototypeAST> ParseExtern(){
  getNextToken();
  return ParsePrototype();
}

static std::unique_ptr<FunctionAST> ParseTopLevelExpr(){
    if(auto E = ParseExpression()){
      auto Proto = std::make_unique<PrototypeAST>("",std::vector<std::string>());
      return std::make_unique<FunctionAST>(std::move(Proto),std::move(E));
    }
    return nullptr;
}



/*********************************************
 * 
 * 
 * 
 * 
 */


static std::unique_ptr<llvm::LLVMContext> TheContext;
static std::unique_ptr<llvm::IRBuilder<>> Builder;
static std::unique_ptr<llvm::Module> TheModule;
static std::map<std::string, llvm::Value*> NamedValues;



llvm::Value *LogErrorV(const char *Str) {
  LogError(Str);
  return nullptr;
}

llvm::Value* NumberExprAst::codegen(){
      return llvm::ConstantFP::get(*TheContext,llvm::APFloat(val));
    }
    
    llvm::Value* VariableExprAst::codegen(){
      llvm::Value* V=NamedValues[name];
      if(!V)
        LogErrorV("Unknown Variable");
      return V;
    }
    llvm::Value* BinaryExprAst::codegen(){
      llvm::Value *L=LHS->codegen();
      llvm::Value *R=RHS->codegen();
      if(!L||!R)
        return nullptr;
      switch (op)
      {
      case '+':
        return Builder->CreateFAdd(L,R,"addtmp");
      case '-':
        return Builder->CreateFSub(L,R,"subtmp");
      case '*':
        return Builder->CreateFMul(L,R,"multmp");
      case '<':
        L=Builder->CreateFCmpULT(L,R,"cmptmp");
        return Builder->CreateUIToFP(L,llvm::Type::getDoubleTy(*TheContext),"booltmp");
      default:
        return LogErrorV("Invalid binary Operator");
      }
      
    }

    llvm::Value* CallExprAST::codegen(){
      llvm::Function *CalleeF=TheModule->getFunction(callee);
      if(!CalleeF)
        return LogErrorV("unknown function refrenced");
      if(CalleeF->arg_size()!=args.size())
        return LogErrorV("Incorrect # arguments passed");
      std::vector<llvm::Value*> argsV;
      for(unsigned i=0,e=args.size();i!=e;i++){
        llvm::Value* v=args[i]->codegen();
        if(!v)
          return nullptr;
        argsV.push_back(v);
      }
      return Builder->CreateCall(CalleeF,argsV,"calltmp");
    }



    llvm::Function* PrototypeAST::codegen(){
      std::vector<llvm::Type*> Doubles(Args.size(),llvm::Type::getDoubleTy(*TheContext));
      llvm::FunctionType *FT=llvm::FunctionType::get(llvm::FunctionType::getDoubleTy(*TheContext),Doubles,false);
      llvm::Function* F=llvm::Function::Create(FT,llvm::Function::ExternalLinkage,Name,TheModule.get());
      unsigned Idx=0;
      for(auto &Arg:F->args()){
        Arg.setName(Args[Idx++]);
      }
      return F;
    }

    llvm::Function* FunctionAST::codegen(){
        llvm::Function* theFunction=TheModule->getFunction(proto->getName());
        if(!theFunction)
          theFunction=proto->codegen();
        if(!theFunction)
          return nullptr;
        if(!theFunction->empty())
          return (llvm::Function*)LogErrorV("Function cannot ve redefined");
        llvm::BasicBlock *BB=llvm::BasicBlock::Create(*TheContext,"entry",theFunction);
        Builder->SetInsertPoint(BB);
        NamedValues.clear();
        for(auto &Arg:theFunction->args())
          NamedValues[std::string(Arg.getName())] = &Arg;
        if(llvm::Value* retVal=body->codegen()){
          Builder->CreateRet(retVal);
          llvm::verifyFunction(*theFunction);
          return theFunction;
        }
        theFunction->eraseFromParent();
        return nullptr;
        
    }
    /*
    
    
    
    */
static void HandleDefinition() {
  if (auto FnAST=ParseDefinition()) {
    if(auto *FnIR = FnAST->codegen()){
        fprintf(stderr, "Read function definition.\n");
        FnIR->print(errs());
        fprintf(stderr, "\n");
        
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleExtern() {
  if (auto ExAST=ParseExtern()) {
    if(auto* ExIR=ExAST->codegen()){
        fprintf(stderr, "Read extern.\n");
        ExIR->print(errs());
        fprintf(stderr, "\n"); 
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (auto TpAST=ParseTopLevelExpr()) {
    if(auto* TpIR=TpAST->codegen()){
        fprintf(stderr, "Read Top Level Expression.\n");
        TpIR->print(errs());
        fprintf(stderr, "\n"); 
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}
static  void initModule(){
  TheContext=std::make_unique<llvm::LLVMContext>();
  TheModule = std::make_unique<llvm::Module>("Kaleidoscope jit",*TheContext);
  Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);
}
static void MainLoop() {
  while (true) {
    fprintf(stderr, "ready> ");
    switch (CurTok) {
    case tok_eof:
      return;
    case ';': // ignore top-level semicolons.
      getNextToken();
      break;
    case tok_def:
      HandleDefinition();
      break;
    case tok_extern:
      HandleExtern();
      break;
    default:
      HandleTopLevelExpression();
      break;
    }
  }
}
int main() {
  // Install standard binary operators.
  // 1 is lowest precedence.
  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40; // highest.

  // Prime the first token.
  fprintf(stderr, "ready> ");
  getNextToken();
  initModule();
  // Run the main "interpreter loop" now.
  MainLoop();

  return 0;
}
//def foo(x y) foo(y, 4.0);