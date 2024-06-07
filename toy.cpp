#include<cctype>
#include<cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>
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

//===----------------------------------------------------------------------===//
// AST
//===----------------------------------------------------------------------===//

// This will create the AST


namespace{
    class ExprAST{
        public:
        virtual ~ExprAST()=default;
    };
    class NumberExprAst : public ExprAST{
        double val;
        public:
            NumberExprAst(double val):val(val){}
    };
    class VariableExprAst : public ExprAST{
        std::string name;
        public:
            VariableExprAst(std::string name):name(name){}
    };
    class BinaryExprAst : public ExprAST{
        char op;
        std::unique_ptr<ExprAST> LHS,RHS;
        public:
            BinaryExprAst(char op,std::unique_ptr<ExprAST> LHS,std::unique_ptr<ExprAST> RHS):op(op),LHS(std::move(LHS)),RHS(std::move(RHS)){}
    };
    class CallExprAST:public ExprAST{
        std::string callee;
        std::vector<std::unique_ptr<ExprAST>> args;
        public:
            CallExprAST(const std::string callee,std::vector<std::unique_ptr<ExprAST>> args):callee(callee),args(move(args)){}
    };
    class PrototypeAST{
        std::string Name;
        std::vector<std::string> Args;
        public:
            PrototypeAST(const std::string &Name,std::vector<std::string> Args):Name(Name),Args(std::move(Args)){}
        const std::string &getName() const { return Name;}

    };
    class FunctionAST
    {
        std::unique_ptr<PrototypeAST> proto;
        std::unique_ptr<ExprAST> body;
        public: 
            FunctionAST(std::unique_ptr<PrototypeAST> proto,std::unique_ptr<ExprAST> body)
            :proto(std::move(proto)),body(std::move(body)){}
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
