#include <iostream>
#include <vector>
#include <string>
#include <unordered_map>
#include <sstream>
#include <fstream>
#include <cctype>
#include <functional>
#include <memory>
#include <cmath>
#include <chrono>
#include <thread>
#include <random>
#include <algorithm>
#include <sys/stat.h>

using namespace std;

// ===================== TOKEN =====================
enum TokenType {
    NUMBER, STRING, IDENT, BOOL,
    PLUS, MINUS, MUL, DIV, MOD,
    ASSIGN, LPAREN, RPAREN,
    LBRACE, RBRACE, LBRACKET, RBRACKET, COMMA, DOT, SEMICOLON,
    IF, ELSE, FOR, TO, UNTIL,
    FUNC, RETURN, PRINT, INPUT,
    EQ, LT, LTE, GT, GTE, NEQ,
    AND, OR, NOT,
    BREAK, CONTINUE,
    INC, DEC, WHILE,
    END
};

struct Token {
    TokenType type;
    string text;
    int line;
};

// ===================== ERROR =====================
class InterpreterException : public exception {
    string msg;
public:
    InterpreterException(const string& m, int l) {
        msg = "Error on line " + to_string(l) + ": " + m;
    }
    const char* what() const noexcept override { return msg.c_str(); }
};

void error(const string& m, int l) {
    throw InterpreterException(m, l);
}

// ===================== VALUE =====================
struct Value {
    enum Type { NUM, STR, BOOL, NONE, FUNC_REF, ARRAY, FILE } type = NONE;
    double num = 0;
    string str;
    bool boolean = false;
    string funcName;
    shared_ptr<vector<Value>> array;
    shared_ptr<fstream> fileHandle;
    string filePath;

    static Value Number(double v) { Value x; x.type = NUM; x.num = v; return x; }
    static Value String(const string& s) { Value x; x.type = STR; x.str = s; return x; }
    static Value Bool(bool b) { Value x; x.type = BOOL; x.boolean = b; return x; }
    static Value None() { Value x; x.type = NONE; return x; }
    static Value FuncRef(const string& n) { Value x; x.type = FUNC_REF; x.funcName = n; return x; }
    static Value Array(vector<Value> items = {}) { 
        Value x; 
        x.type = ARRAY; 
        x.array = make_shared<vector<Value>>(items); 
        return x; 
    }
    static Value File(const string& path, shared_ptr<fstream> handle) {
        Value x;
        x.type = FILE;
        x.filePath = path;
        x.fileHandle = handle;
        return x;
    }

    string toString() const {
        if (type == STR) return str;
        if (type == NUM) {
            double intpart;
            if (modf(num, &intpart) == 0.0) {
                return to_string((long long)num);
            }
            return to_string(num);
        }
        if (type == BOOL) return boolean ? "yes" : "no";
        if (type == NONE) return "none";
        if (type == FUNC_REF) return "<function:" + funcName + ">";
        if (type == ARRAY) {
            string result = "[";
            for (size_t i = 0; i < array->size(); i++) {
                if (i > 0) result += ", ";
                result += (*array)[i].toString();
            }
            result += "]";
            return result;
        }
        if (type == FILE) return "<file:" + filePath + ">";
        return "";
    }

    double toNumber() const {
        if (type == NUM) return num;
        if (type == BOOL) return boolean ? 1 : 0;
        if (type == ARRAY) return array->size();
        if (type == STR) {
            try { return stod(str); }
            catch (...) { return 0; }
        }
        return 0;
    }

    bool toBool() const {
        if (type == BOOL) return boolean;
        if (type == NUM) return num != 0;
        if (type == STR) return !str.empty();
        if (type == ARRAY) return !array->empty();
        return false;
    }
};

// ===================== AST NODES =====================
struct ASTNode {
    virtual ~ASTNode() = default;
};

struct NumberNode : ASTNode {
    double value;
    NumberNode(double v) : value(v) {}
};

struct StringNode : ASTNode {
    string value;
    StringNode(const string& v) : value(v) {}
};

struct BoolNode : ASTNode {
    bool value;
    BoolNode(bool v) : value(v) {}
};

struct VarNode : ASTNode {
    string name;
    VarNode(const string& n) : name(n) {}
};

struct BinOpNode : ASTNode {
    TokenType op;
    shared_ptr<ASTNode> left, right;
    BinOpNode(TokenType o, shared_ptr<ASTNode> l, shared_ptr<ASTNode> r) 
        : op(o), left(l), right(r) {}
};

struct UnaryOpNode : ASTNode {
    TokenType op;
    shared_ptr<ASTNode> operand;
    UnaryOpNode(TokenType o, shared_ptr<ASTNode> n) : op(o), operand(n) {}
};

struct AssignNode : ASTNode {
    string name;
    shared_ptr<ASTNode> value;
    AssignNode(const string& n, shared_ptr<ASTNode> v) : name(n), value(v) {}
};

struct CallNode : ASTNode {
    string name;
    vector<shared_ptr<ASTNode>> args;
    CallNode(const string& n, vector<shared_ptr<ASTNode>> a) : name(n), args(a) {}
};

struct MethodCallNode : ASTNode {
    string objName;
    string method;
    vector<shared_ptr<ASTNode>> args;
    MethodCallNode(const string& o, const string& m, vector<shared_ptr<ASTNode>> a) 
        : objName(o), method(m), args(a) {}
};

struct BlockNode : ASTNode {
    vector<shared_ptr<ASTNode>> statements;
    BlockNode(vector<shared_ptr<ASTNode>> s) : statements(s) {}
};

struct IfNode : ASTNode {
    shared_ptr<ASTNode> condition;
    shared_ptr<ASTNode> thenBlock;
    shared_ptr<ASTNode> elseBlock;
    IfNode(shared_ptr<ASTNode> c, shared_ptr<ASTNode> t, shared_ptr<ASTNode> e = nullptr) 
        : condition(c), thenBlock(t), elseBlock(e) {}
};

struct WhileNode : ASTNode {
    shared_ptr<ASTNode> condition;
    shared_ptr<ASTNode> body;
    bool isUntil;
    WhileNode(shared_ptr<ASTNode> c, shared_ptr<ASTNode> b, bool u = false) 
        : condition(c), body(b), isUntil(u) {}
};

struct ForNode : ASTNode {
    string var;
    shared_ptr<ASTNode> start;
    shared_ptr<ASTNode> end;
    shared_ptr<ASTNode> body;
    ForNode(const string& v, shared_ptr<ASTNode> s, shared_ptr<ASTNode> e, shared_ptr<ASTNode> b) 
        : var(v), start(s), end(e), body(b) {}
};

struct FuncDefNode : ASTNode {
    string name;
    vector<string> params;
    shared_ptr<ASTNode> body;
    FuncDefNode(const string& n, vector<string> p, shared_ptr<ASTNode> b) 
        : name(n), params(p), body(b) {}
};

struct ReturnNode : ASTNode {
    shared_ptr<ASTNode> value;
    ReturnNode(shared_ptr<ASTNode> v) : value(v) {}
};

struct PrintNode : ASTNode {
    shared_ptr<ASTNode> value;
    PrintNode(shared_ptr<ASTNode> v) : value(v) {}
};

struct InputNode : ASTNode {
    string varName;
    InputNode(const string& n) : varName(n) {}
};

struct ArrayLiteralNode : ASTNode {
    vector<shared_ptr<ASTNode>> elements;
    ArrayLiteralNode(vector<shared_ptr<ASTNode>> e) : elements(e) {}
};

struct IndexAccessNode : ASTNode {
    string arrayName;
    shared_ptr<ASTNode> index;
    IndexAccessNode(const string& n, shared_ptr<ASTNode> i) : arrayName(n), index(i) {}
};

struct IndexAssignNode : ASTNode {
    string arrayName;
    shared_ptr<ASTNode> index;
    shared_ptr<ASTNode> value;
    IndexAssignNode(const string& n, shared_ptr<ASTNode> i, shared_ptr<ASTNode> v) 
        : arrayName(n), index(i), value(v) {}
};

struct BreakNode : ASTNode {};
struct ContinueNode : ASTNode {};

// ===================== LEXER =====================
vector<Token> tokenize(const string& src) {
    vector<Token> t;
    int line = 1;

    for (size_t i = 0; i < src.size();) {
        if (src[i] == '\n') { line++; i++; continue; }
        if (isspace(src[i])) { i++; continue; }

        // Comments
        if (src[i] == '#') {
            while (i < src.size() && src[i] != '\n') i++;
            continue;
        }

        // Numbers
        if (isdigit(src[i])) {
            string n;
            while (i < src.size() && (isdigit(src[i]) || src[i] == '.'))
                n += src[i++];
            t.push_back({ NUMBER, n, line });
            continue;
        }

        // Strings
        if (src[i] == '"') {
            i++;
            string s;
            while (i < src.size() && src[i] != '"') {
                if (src[i] == '\\' && i + 1 < src.size()) {
                    i++;
                    if (src[i] == 'n') s += '\n';
                    else if (src[i] == 't') s += '\t';
                    else s += src[i];
                    i++;
                } else {
                    s += src[i++];
                }
            }
            i++;
            t.push_back({ STRING, s, line });
            continue;
        }

        // Identifiers and keywords
        if (isalpha(src[i]) || src[i] == '_') {
            string id;
            while (i < src.size() && (isalnum(src[i]) || src[i] == '_'))
                id += src[i++];

            if (id == "when") t.push_back({ IF, id, line });
            else if (id == "other") t.push_back({ ELSE, id, line });
            else if (id == "repeat") t.push_back({ FOR, id, line });
            else if (id == "loop") t.push_back({ WHILE, id, line });
            else if (id == "to") t.push_back({ TO, id, line });
            else if (id == "until") t.push_back({ UNTIL, id, line });
            else if (id == "task") t.push_back({ FUNC, id, line });
            else if (id == "give") t.push_back({ RETURN, id, line });
            else if (id == "out") t.push_back({ PRINT, id, line });
            else if (id == "in") t.push_back({ INPUT, id, line });
            else if (id == "yes") t.push_back({ BOOL, "1", line });
            else if (id == "no") t.push_back({ BOOL, "0", line });
            else if (id == "stop") t.push_back({ BREAK, id, line });
            else if (id == "skip") t.push_back({ CONTINUE, id, line });
            else if (id == "and") t.push_back({ AND, id, line });
            else if (id == "or") t.push_back({ OR, id, line });
            else if (id == "not") t.push_back({ NOT, id, line });
            else t.push_back({ IDENT, id, line });
            continue;
        }

        // Two-character operators
        if (i + 1 < src.size()) {
            string two = string() + src[i] + src[i + 1];
            if (two == "==") { t.push_back({ EQ, two, line }); i += 2; continue; }
            if (two == "!=") { t.push_back({ NEQ, two, line }); i += 2; continue; }
            if (two == "<=") { t.push_back({ LTE, two, line }); i += 2; continue; }
            if (two == ">=") { t.push_back({ GTE, two, line }); i += 2; continue; }
            if (two == "++") { t.push_back({ INC, two, line }); i += 2; continue; }
            if (two == "--") { t.push_back({ DEC, two, line }); i += 2; continue; }
        }

        // Single-character operators
        if (src[i] == '.') { t.push_back({ DOT, ".", line }); i++; continue; }
        if (src[i] == '+') { t.push_back({ PLUS, "+", line }); i++; continue; }
        if (src[i] == '-') { t.push_back({ MINUS, "-", line }); i++; continue; }
        if (src[i] == '*') { t.push_back({ MUL, "*", line }); i++; continue; }
        if (src[i] == '/') { t.push_back({ DIV, "/", line }); i++; continue; }
        if (src[i] == '%') { t.push_back({ MOD, "%", line }); i++; continue; }
        if (src[i] == '=') { t.push_back({ ASSIGN, "=", line }); i++; continue; }
        if (src[i] == '(') { t.push_back({ LPAREN, "(", line }); i++; continue; }
        if (src[i] == ')') { t.push_back({ RPAREN, ")", line }); i++; continue; }
        if (src[i] == '{') { t.push_back({ LBRACE, "{", line }); i++; continue; }
        if (src[i] == '}') { t.push_back({ RBRACE, "}", line }); i++; continue; }
        if (src[i] == '[') { t.push_back({ LBRACKET, "[", line }); i++; continue; }
        if (src[i] == ']') { t.push_back({ RBRACKET, "]", line }); i++; continue; }
        if (src[i] == ',') { t.push_back({ COMMA, ",", line }); i++; continue; }
        if (src[i] == ';') { t.push_back({ SEMICOLON, ";", line }); i++; continue; }
        if (src[i] == '<') { t.push_back({ LT, "<", line }); i++; continue; }
        if (src[i] == '>') { t.push_back({ GT, ">", line }); i++; continue; }

        error("Unknown character: " + string(1, src[i]), line);
    }

    t.push_back({ END, "", line });
    return t;
}

// ===================== PARSER =====================
class Parser {
    vector<Token> tokens;
    size_t pos = 0;

    Token current() {
        if (pos < tokens.size()) return tokens[pos];
        return tokens.back();
    }

    void advance() {
        if (pos < tokens.size()) pos++;
    }

    bool match(TokenType type) {
        if (current().type == type) {
            advance();
            return true;
        }
        return false;
    }

    void expect(TokenType type, const string& msg) {
        if (!match(type)) {
            error(msg, current().line);
        }
    }

    shared_ptr<ASTNode> primary() {
        if (current().type == NUMBER) {
            double val = stod(current().text);
            advance();
            return make_shared<NumberNode>(val);
        }

        if (current().type == STRING) {
            string val = current().text;
            advance();
            return make_shared<StringNode>(val);
        }

        if (current().type == BOOL) {
            bool val = current().text == "1";
            advance();
            return make_shared<BoolNode>(val);
        }

        if (current().type == LBRACKET) {
            advance();
            vector<shared_ptr<ASTNode>> elements;
            
            while (current().type != RBRACKET) {
                elements.push_back(expression());
                if (current().type == COMMA) advance();
            }
            
            expect(RBRACKET, "Expected ']'");
            return make_shared<ArrayLiteralNode>(elements);
        }

        if (current().type == LPAREN) {
            advance();
            auto expr = expression();
            expect(RPAREN, "Expected ')'");
            return expr;
        }

        if (current().type == NOT) {
            advance();
            return make_shared<UnaryOpNode>(NOT, primary());
        }

        if (current().type == MINUS) {
            advance();
            return make_shared<UnaryOpNode>(MINUS, primary());
        }

        if (current().type == IDENT) {
            string name = current().text;
            advance();

            // Array index access
            if (current().type == LBRACKET) {
                advance();
                auto index = expression();
                expect(RBRACKET, "Expected ']'");
                return make_shared<IndexAccessNode>(name, index);
            }

            // Function call
            if (current().type == LPAREN) {
                advance();
                vector<shared_ptr<ASTNode>> args;
                while (current().type != RPAREN) {
                    args.push_back(expression());
                    if (current().type == COMMA) advance();
                }
                expect(RPAREN, "Expected ')'");
                return make_shared<CallNode>(name, args);
            }

            // Method call
            if (current().type == DOT) {
                advance();
                string method = current().text;
                expect(IDENT, "Expected method name");
                expect(LPAREN, "Expected '('");
                vector<shared_ptr<ASTNode>> args;
                while (current().type != RPAREN) {
                    args.push_back(expression());
                    if (current().type == COMMA) advance();
                }
                expect(RPAREN, "Expected ')'");
                return make_shared<MethodCallNode>(name, method, args);
            }

            return make_shared<VarNode>(name);
        }

        error("Unexpected token: " + current().text, current().line);
        return nullptr;
    }

    shared_ptr<ASTNode> term() {
        auto node = primary();

        while (current().type == MUL || current().type == DIV || current().type == MOD) {
            TokenType op = current().type;
            advance();
            node = make_shared<BinOpNode>(op, node, primary());
        }

        return node;
    }

    shared_ptr<ASTNode> arithmetic() {
        auto node = term();

        while (current().type == PLUS || current().type == MINUS) {
            TokenType op = current().type;
            advance();
            node = make_shared<BinOpNode>(op, node, term());
        }

        return node;
    }

    shared_ptr<ASTNode> comparison() {
        auto node = arithmetic();

        while (current().type == EQ || current().type == NEQ || 
               current().type == LT || current().type == GT ||
               current().type == LTE || current().type == GTE) {
            TokenType op = current().type;
            advance();
            node = make_shared<BinOpNode>(op, node, arithmetic());
        }

        return node;
    }

    shared_ptr<ASTNode> logical() {
        auto node = comparison();

        while (current().type == AND || current().type == OR) {
            TokenType op = current().type;
            advance();
            node = make_shared<BinOpNode>(op, node, comparison());
        }

        return node;
    }

    shared_ptr<ASTNode> expression() {
        return logical();
    }

    shared_ptr<ASTNode> statement() {
        // Print statement
        if (match(PRINT)) {
            return make_shared<PrintNode>(expression());
        }

        // Input statement
        if (match(INPUT)) {
            string name = current().text;
            expect(IDENT, "Expected variable name");
            return make_shared<InputNode>(name);
        }

        // Return statement
        if (match(RETURN)) {
            if (current().type == RBRACE || current().type == END) {
                return make_shared<ReturnNode>(nullptr);
            }
            return make_shared<ReturnNode>(expression());
        }

        // Break statement
        if (match(BREAK)) {
            return make_shared<BreakNode>();
        }

        // Continue statement
        if (match(CONTINUE)) {
            return make_shared<ContinueNode>();
        }

        // If statement
        if (match(IF)) {
            auto cond = expression();
            expect(LBRACE, "Expected '{'");
            auto thenBlock = block();
            shared_ptr<ASTNode> elseBlock = nullptr;
            
            if (match(ELSE)) {
                expect(LBRACE, "Expected '{'");
                elseBlock = block();
            }
            
            return make_shared<IfNode>(cond, thenBlock, elseBlock);
        }

        // While loop
        if (match(WHILE)) {
            auto cond = expression();
            expect(LBRACE, "Expected '{'");
            auto body = block();
            return make_shared<WhileNode>(cond, body, false);
        }

        // Until loop
        if (match(UNTIL)) {
            auto cond = expression();
            expect(LBRACE, "Expected '{'");
            auto body = block();
            return make_shared<WhileNode>(cond, body, true);
        }

        // For loop
        if (match(FOR)) {
            string var = current().text;
            expect(IDENT, "Expected variable name");
            expect(ASSIGN, "Expected '='");
            auto start = expression();
            expect(TO, "Expected 'to'");
            auto end = expression();
            expect(LBRACE, "Expected '{'");
            auto body = block();
            return make_shared<ForNode>(var, start, end, body);
        }

        // Function definition
        if (match(FUNC)) {
            string name = current().text;
            expect(IDENT, "Expected function name");
            expect(LPAREN, "Expected '('");
            
            vector<string> params;
            while (current().type != RPAREN) {
                params.push_back(current().text);
                expect(IDENT, "Expected parameter name");
                if (current().type == COMMA) advance();
            }
            expect(RPAREN, "Expected ')'");
            expect(LBRACE, "Expected '{'");
            auto body = block();
            
            return make_shared<FuncDefNode>(name, params, body);
        }

        // Assignment or expression statement
        if (current().type == IDENT) {
            string name = current().text;
            advance();

            // Array index assignment
            if (current().type == LBRACKET) {
                advance();
                auto index = expression();
                expect(RBRACKET, "Expected ']'");
                expect(ASSIGN, "Expected '='");
                auto value = expression();
                return make_shared<IndexAssignNode>(name, index, value);
            }

            if (match(ASSIGN)) {
                auto value = expression();
                return make_shared<AssignNode>(name, value);
            } else if (match(INC)) {
                return make_shared<AssignNode>(name, 
                    make_shared<BinOpNode>(PLUS, make_shared<VarNode>(name), make_shared<NumberNode>(1)));
            } else if (match(DEC)) {
                return make_shared<AssignNode>(name, 
                    make_shared<BinOpNode>(MINUS, make_shared<VarNode>(name), make_shared<NumberNode>(1)));
            } else {
                pos--; // Backtrack
                return expression();
            }
        }

        error("Unexpected token in statement: " + current().text, current().line);
        return nullptr;
    }

    shared_ptr<ASTNode> block() {
        vector<shared_ptr<ASTNode>> statements;
        
        while (current().type != RBRACE && current().type != END) {
            statements.push_back(statement());
        }
        
        expect(RBRACE, "Expected '}'");
        return make_shared<BlockNode>(statements);
    }

public:
    Parser(vector<Token> t) : tokens(t) {}

    vector<shared_ptr<ASTNode>> parse() {
        vector<shared_ptr<ASTNode>> statements;
        
        while (current().type != END) {
            statements.push_back(statement());
        }
        
        return statements;
    }
};

// ===================== INTERPRETER =====================
struct ReturnValue : exception {
    Value value;
    ReturnValue(Value v) : value(v) {}
};

struct BreakSignal : exception {};
struct ContinueSignal : exception {};

struct UserFunction {
    vector<string> params;
    shared_ptr<ASTNode> body;
};

class Interpreter {
    vector<unordered_map<string, Value>> scopes;
    unordered_map<string, UserFunction> functions;
    unordered_map<string, function<Value(vector<Value>&)>> nativeFuncs;

    void pushScope() {
        scopes.push_back({});
    }

    void popScope() {
        if (!scopes.empty()) scopes.pop_back();
    }

    void setVar(const string& name, Value val) {
        if (!scopes.empty()) {
            scopes.back()[name] = val;
        }
    }

    Value getVar(const string& name) {
        for (int i = scopes.size() - 1; i >= 0; i--) {
            if (scopes[i].count(name)) {
                return scopes[i][name];
            }
        }
        return Value::None();
    }

    Value eval(shared_ptr<ASTNode> node) {
        if (!node) return Value::None();

        if (auto n = dynamic_cast<NumberNode*>(node.get())) {
            return Value::Number(n->value);
        }

        if (auto n = dynamic_cast<StringNode*>(node.get())) {
            return Value::String(n->value);
        }

        if (auto n = dynamic_cast<BoolNode*>(node.get())) {
            return Value::Bool(n->value);
        }

        if (auto n = dynamic_cast<VarNode*>(node.get())) {
            return getVar(n->name);
        }

        if (auto n = dynamic_cast<BinOpNode*>(node.get())) {
            Value left = eval(n->left);
            Value right = eval(n->right);

            switch (n->op) {
                case PLUS:
                    if (left.type == Value::STR || right.type == Value::STR) {
                        return Value::String(left.toString() + right.toString());
                    }
                    return Value::Number(left.toNumber() + right.toNumber());
                case MINUS: return Value::Number(left.toNumber() - right.toNumber());
                case MUL: return Value::Number(left.toNumber() * right.toNumber());
                case DIV: 
                    if (right.toNumber() == 0) error("Division by zero", 0);
                    return Value::Number(left.toNumber() / right.toNumber());
                case MOD: return Value::Number(fmod(left.toNumber(), right.toNumber()));
                case EQ: 
                    if (left.type == Value::STR && right.type == Value::STR) {
                        return Value::Bool(left.str == right.str);
                    }
                    return Value::Bool(left.toNumber() == right.toNumber());
                case NEQ:
                    if (left.type == Value::STR && right.type == Value::STR) {
                        return Value::Bool(left.str != right.str);
                    }
                    return Value::Bool(left.toNumber() != right.toNumber());
                case LT: return Value::Bool(left.toNumber() < right.toNumber());
                case GT: return Value::Bool(left.toNumber() > right.toNumber());
                case LTE: return Value::Bool(left.toNumber() <= right.toNumber());
                case GTE: return Value::Bool(left.toNumber() >= right.toNumber());
                case AND: return Value::Bool(left.toBool() && right.toBool());
                case OR: return Value::Bool(left.toBool() || right.toBool());
                default: return Value::None();
            }
        }

        if (auto n = dynamic_cast<UnaryOpNode*>(node.get())) {
            Value operand = eval(n->operand);
            if (n->op == NOT) return Value::Bool(!operand.toBool());
            if (n->op == MINUS) return Value::Number(-operand.toNumber());
        }

        if (auto n = dynamic_cast<AssignNode*>(node.get())) {
            Value val = eval(n->value);
            setVar(n->name, val);
            return val;
        }

        if (auto n = dynamic_cast<CallNode*>(node.get())) {
            vector<Value> args;
            for (auto& arg : n->args) {
                args.push_back(eval(arg));
            }

            // Check native functions
            if (nativeFuncs.count(n->name)) {
                return nativeFuncs[n->name](args);
            }

            // Check user functions
            if (functions.count(n->name)) {
                auto& func = functions[n->name];
                pushScope();
                
                for (size_t i = 0; i < func.params.size() && i < args.size(); i++) {
                    setVar(func.params[i], args[i]);
                }

                try {
                    eval(func.body);
                } catch (ReturnValue& ret) {
                    popScope();
                    return ret.value;
                }

                popScope();
                return Value::None();
            }

            error("Unknown function: " + n->name, 0);
        }

        if (auto n = dynamic_cast<MethodCallNode*>(node.get())) {
            Value obj = getVar(n->objName);
            vector<Value> args;
            for (auto& arg : n->args) {
                args.push_back(eval(arg));
            }

            // String methods
            if (n->method == "length" && obj.type == Value::STR) {
                return Value::Number(obj.str.length());
            }
            if (n->method == "upper" && obj.type == Value::STR) {
                string s = obj.str;
                for (auto& c : s) c = toupper(c);
                return Value::String(s);
            }
            if (n->method == "lower" && obj.type == Value::STR) {
                string s = obj.str;
                for (auto& c : s) c = tolower(c);
                return Value::String(s);
            }

            // Array methods
            if (obj.type == Value::ARRAY) {
                if (n->method == "push") {
                    for (auto& arg : args) {
                        obj.array->push_back(arg);
                    }
                    return Value::None();
                }
                if (n->method == "pop") {
                    if (!obj.array->empty()) {
                        Value last = obj.array->back();
                        obj.array->pop_back();
                        return last;
                    }
                    return Value::None();
                }
                if (n->method == "length" || n->method == "size") {
                    return Value::Number(obj.array->size());
                }
                if (n->method == "clear") {
                    obj.array->clear();
                    return Value::None();
                }
                if (n->method == "get" && !args.empty()) {
                    int idx = (int)args[0].toNumber();
                    if (idx >= 0 && idx < (int)obj.array->size()) {
                        return (*obj.array)[idx];
                    }
                    return Value::None();
                }
            }

            // File methods
            if (obj.type == Value::FILE) {
                if (n->method == "add" || n->method == "write") {
                    if (obj.fileHandle && obj.fileHandle->is_open()) {
                        for (auto& arg : args) {
                            (*obj.fileHandle) << arg.toString();
                        }
                        obj.fileHandle->flush();
                    }
                    return Value::None();
                }
                if (n->method == "readLine") {
                    if (obj.fileHandle && obj.fileHandle->is_open()) {
                        string line;
                        if (getline(*obj.fileHandle, line)) {
                            return Value::String(line);
                        }
                    }
                    return Value::None();
                }
                if (n->method == "readAll") {
                    if (obj.fileHandle && obj.fileHandle->is_open()) {
                        stringstream ss;
                        ss << obj.fileHandle->rdbuf();
                        return Value::String(ss.str());
                    }
                    return Value::None();
                }
                if (n->method == "close") {
                    if (obj.fileHandle && obj.fileHandle->is_open()) {
                        obj.fileHandle->close();
                    }
                    return Value::None();
                }
            }

            error("Unknown method: " + n->method, 0);
        }

        if (auto n = dynamic_cast<ArrayLiteralNode*>(node.get())) {
            vector<Value> elements;
            for (auto& elem : n->elements) {
                elements.push_back(eval(elem));
            }
            return Value::Array(elements);
        }

        if (auto n = dynamic_cast<IndexAccessNode*>(node.get())) {
            Value arr = getVar(n->arrayName);
            if (arr.type != Value::ARRAY) {
                error("Cannot index non-array type", 0);
            }
            
            int idx = (int)eval(n->index).toNumber();
            if (idx < 0 || idx >= (int)arr.array->size()) {
                error("Array index out of bounds: " + to_string(idx), 0);
            }
            
            return (*arr.array)[idx];
        }

        if (auto n = dynamic_cast<IndexAssignNode*>(node.get())) {
            Value arr = getVar(n->arrayName);
            if (arr.type != Value::ARRAY) {
                error("Cannot index non-array type", 0);
            }
            
            int idx = (int)eval(n->index).toNumber();
            if (idx < 0 || idx >= (int)arr.array->size()) {
                error("Array index out of bounds: " + to_string(idx), 0);
            }
            
            Value val = eval(n->value);
            (*arr.array)[idx] = val;
            return val;
        }

        if (auto n = dynamic_cast<BlockNode*>(node.get())) {
            Value last = Value::None();
            for (auto& stmt : n->statements) {
                last = eval(stmt);
            }
            return last;
        }

        if (auto n = dynamic_cast<IfNode*>(node.get())) {
            Value cond = eval(n->condition);
            if (cond.toBool()) {
                return eval(n->thenBlock);
            } else if (n->elseBlock) {
                return eval(n->elseBlock);
            }
            return Value::None();
        }

        if (auto n = dynamic_cast<WhileNode*>(node.get())) {
            while (true) {
                Value cond = eval(n->condition);
                bool shouldRun = n->isUntil ? !cond.toBool() : cond.toBool();
                
                if (!shouldRun) break;

                try {
                    eval(n->body);
                } catch (BreakSignal&) {
                    break;
                } catch (ContinueSignal&) {
                    continue;
                }
            }
            return Value::None();
        }

        if (auto n = dynamic_cast<ForNode*>(node.get())) {
            Value start = eval(n->start);
            Value end = eval(n->end);
            
            for (double i = start.toNumber(); i <= end.toNumber(); i++) {
                setVar(n->var, Value::Number(i));
                
                try {
                    eval(n->body);
                } catch (BreakSignal&) {
                    break;
                } catch (ContinueSignal&) {
                    continue;
                }
            }
            return Value::None();
        }

        if (auto n = dynamic_cast<FuncDefNode*>(node.get())) {
            functions[n->name] = { n->params, n->body };
            return Value::None();
        }

        if (auto n = dynamic_cast<ReturnNode*>(node.get())) {
            Value val = n->value ? eval(n->value) : Value::None();
            throw ReturnValue(val);
        }

        if (auto n = dynamic_cast<PrintNode*>(node.get())) {
            cout << eval(n->value).toString() << endl;
            return Value::None();
        }

        if (auto n = dynamic_cast<InputNode*>(node.get())) {
            string input;
            getline(cin, input);
            setVar(n->varName, Value::String(input));
            return Value::None();
        }

        if (dynamic_cast<BreakNode*>(node.get())) {
            throw BreakSignal();
        }

        if (dynamic_cast<ContinueNode*>(node.get())) {
            throw ContinueSignal();
        }

        return Value::None();
    }

public:
    Interpreter() {
        pushScope(); // Global scope

        // ==================== FILE I/O ====================
        nativeFuncs["read"] = [](vector<Value>& args) {
            if (args.empty()) return Value::String("");
            ifstream f(args[0].toString());
            if (!f) return Value::String("");
            stringstream ss;
            ss << f.rdbuf();
            return Value::String(ss.str());
        };

        nativeFuncs["write"] = [](vector<Value>& args) {
            if (args.size() < 2) return Value::None();
            auto handle = make_shared<fstream>(args[0].toString(), ios::out | ios::trunc);
            if (handle->is_open()) {
                (*handle) << args[1].toString();
            }
            return Value::File(args[0].toString(), handle);
        };

        nativeFuncs["append"] = [](vector<Value>& args) {
            if (args.size() < 2) return Value::None();
            auto handle = make_shared<fstream>(args[0].toString(), ios::out | ios::app);
            if (handle->is_open()) {
                (*handle) << args[1].toString();
            }
            return Value::File(args[0].toString(), handle);
        };

        nativeFuncs["open"] = [](vector<Value>& args) {
            if (args.empty()) return Value::None();
            string mode = args.size() > 1 ? args[1].toString() : "r";
            auto handle = make_shared<fstream>();
            
            if (mode == "r") {
                handle->open(args[0].toString(), ios::in);
            } else if (mode == "w") {
                handle->open(args[0].toString(), ios::out | ios::trunc);
            } else if (mode == "a") {
                handle->open(args[0].toString(), ios::out | ios::app);
            } else if (mode == "rw") {
                handle->open(args[0].toString(), ios::in | ios::out);
            }
            
            return Value::File(args[0].toString(), handle);
        };

        nativeFuncs["exists"] = [](vector<Value>& args) {
            if (args.empty()) return Value::Bool(false);
            struct stat buffer;
            return Value::Bool(stat(args[0].toString().c_str(), &buffer) == 0);
        };

        // ==================== MATH ====================
        nativeFuncs["abs"] = [](vector<Value>& args) {
            if (args.empty()) return Value::Number(0);
            return Value::Number(abs(args[0].toNumber()));
        };

        nativeFuncs["sqrt"] = [](vector<Value>& args) {
            if (args.empty()) return Value::Number(0);
            return Value::Number(sqrt(args[0].toNumber()));
        };

        nativeFuncs["pow"] = [](vector<Value>& args) {
            if (args.size() < 2) return Value::Number(0);
            return Value::Number(pow(args[0].toNumber(), args[1].toNumber()));
        };

        nativeFuncs["floor"] = [](vector<Value>& args) {
            if (args.empty()) return Value::Number(0);
            return Value::Number(floor(args[0].toNumber()));
        };

        nativeFuncs["ceil"] = [](vector<Value>& args) {
            if (args.empty()) return Value::Number(0);
            return Value::Number(ceil(args[0].toNumber()));
        };

        nativeFuncs["round"] = [](vector<Value>& args) {
            if (args.empty()) return Value::Number(0);
            return Value::Number(round(args[0].toNumber()));
        };

        nativeFuncs["random"] = [](vector<Value>& args) {
            static random_device rd;
            static mt19937 gen(rd());
            
            if (args.empty()) {
                uniform_real_distribution<> dis(0.0, 1.0);
                return Value::Number(dis(gen));
            }
            
            int min = 0, max = 100;
            if (args.size() == 1) {
                max = (int)args[0].toNumber();
            } else if (args.size() >= 2) {
                min = (int)args[0].toNumber();
                max = (int)args[1].toNumber();
            }
            
            uniform_int_distribution<> dis(min, max);
            return Value::Number(dis(gen));
        };

        nativeFuncs["min"] = [](vector<Value>& args) {
            if (args.empty()) return Value::Number(0);
            double minVal = args[0].toNumber();
            for (auto& arg : args) {
                minVal = std::min(minVal, arg.toNumber());
            }
            return Value::Number(minVal);
        };

        nativeFuncs["max"] = [](vector<Value>& args) {
            if (args.empty()) return Value::Number(0);
            double maxVal = args[0].toNumber();
            for (auto& arg : args) {
                maxVal = std::max(maxVal, arg.toNumber());
            }
            return Value::Number(maxVal);
        };

        // ==================== STRING ====================
        nativeFuncs["len"] = [](vector<Value>& args) {
            if (args.empty()) return Value::Number(0);
            if (args[0].type == Value::ARRAY) {
                return Value::Number(args[0].array->size());
            }
            if (args[0].type == Value::STR) {
                return Value::Number(args[0].str.length());
            }
            return Value::Number(args[0].toString().length());
        };

        nativeFuncs["upper"] = [](vector<Value>& args) {
            if (args.empty()) return Value::String("");
            string s = args[0].toString();
            transform(s.begin(), s.end(), s.begin(), ::toupper);
            return Value::String(s);
        };

        nativeFuncs["lower"] = [](vector<Value>& args) {
            if (args.empty()) return Value::String("");
            string s = args[0].toString();
            transform(s.begin(), s.end(), s.begin(), ::tolower);
            return Value::String(s);
        };

        nativeFuncs["substr"] = [](vector<Value>& args) {
            if (args.size() < 2) return Value::String("");
            string s = args[0].toString();
            int start = (int)args[1].toNumber();
            int length = args.size() > 2 ? (int)args[2].toNumber() : s.length() - start;
            
            if (start < 0 || start >= (int)s.length()) return Value::String("");
            return Value::String(s.substr(start, length));
        };

        nativeFuncs["split"] = [](vector<Value>& args) {
            if (args.empty()) return Value::Array();
            string s = args[0].toString();
            string delim = args.size() > 1 ? args[1].toString() : " ";
            
            vector<Value> result;
            size_t start = 0, end = 0;
            
            while ((end = s.find(delim, start)) != string::npos) {
                result.push_back(Value::String(s.substr(start, end - start)));
                start = end + delim.length();
            }
            result.push_back(Value::String(s.substr(start)));
            
            return Value::Array(result);
        };

        nativeFuncs["replace"] = [](vector<Value>& args) {
            if (args.size() < 3) return Value::String("");
            string s = args[0].toString();
            string oldStr = args[1].toString();
            string newStr = args[2].toString();
            
            size_t pos = 0;
            while ((pos = s.find(oldStr, pos)) != string::npos) {
                s.replace(pos, oldStr.length(), newStr);
                pos += newStr.length();
            }
            
            return Value::String(s);
        };

        nativeFuncs["trim"] = [](vector<Value>& args) {
            if (args.empty()) return Value::String("");
            string s = args[0].toString();
            
            size_t start = s.find_first_not_of(" \t\n\r");
            size_t end = s.find_last_not_of(" \t\n\r");
            
            if (start == string::npos) return Value::String("");
            return Value::String(s.substr(start, end - start + 1));
        };

        nativeFuncs["charAt"] = [](vector<Value>& args) {
            if (args.size() < 2) return Value::String("");
            string s = args[0].toString();
            int idx = (int)args[1].toNumber();
            
            if (idx < 0 || idx >= (int)s.length()) return Value::String("");
            return Value::String(string(1, s[idx]));
        };

        // ==================== ARRAY ====================
        nativeFuncs["push"] = [](vector<Value>& args) {
            if (args.size() < 2) return Value::None();
            if (args[0].type != Value::ARRAY) return Value::None();
            
            for (size_t i = 1; i < args.size(); i++) {
                args[0].array->push_back(args[i]);
            }
            return Value::None();
        };

        nativeFuncs["pop"] = [](vector<Value>& args) {
            if (args.empty() || args[0].type != Value::ARRAY) return Value::None();
            if (args[0].array->empty()) return Value::None();
            
            Value last = args[0].array->back();
            args[0].array->pop_back();
            return last;
        };

        nativeFuncs["insert"] = [](vector<Value>& args) {
            if (args.size() < 3 || args[0].type != Value::ARRAY) return Value::None();
            
            int idx = (int)args[1].toNumber();
            if (idx < 0 || idx > (int)args[0].array->size()) return Value::None();
            
            args[0].array->insert(args[0].array->begin() + idx, args[2]);
            return Value::None();
        };

        nativeFuncs["remove"] = [](vector<Value>& args) {
            if (args.size() < 2 || args[0].type != Value::ARRAY) return Value::None();
            
            int idx = (int)args[1].toNumber();
            if (idx < 0 || idx >= (int)args[0].array->size()) return Value::None();
            
            Value removed = (*args[0].array)[idx];
            args[0].array->erase(args[0].array->begin() + idx);
            return removed;
        };

        nativeFuncs["indexOf"] = [](vector<Value>& args) {
            if (args.size() < 2 || args[0].type != Value::ARRAY) return Value::Number(-1);
            
            for (size_t i = 0; i < args[0].array->size(); i++) {
                Value& elem = (*args[0].array)[i];
                if (elem.type == args[1].type) {
                    if (elem.type == Value::NUM && elem.num == args[1].num) return Value::Number(i);
                    if (elem.type == Value::STR && elem.str == args[1].str) return Value::Number(i);
                    if (elem.type == Value::BOOL && elem.boolean == args[1].boolean) return Value::Number(i);
                }
            }
            
            return Value::Number(-1);
        };

        nativeFuncs["join"] = [](vector<Value>& args) {
            if (args.empty() || args[0].type != Value::ARRAY) return Value::String("");
            string delim = args.size() > 1 ? args[1].toString() : ",";
            
            string result;
            for (size_t i = 0; i < args[0].array->size(); i++) {
                if (i > 0) result += delim;
                result += (*args[0].array)[i].toString();
            }
            
            return Value::String(result);
        };

        nativeFuncs["reverse"] = [](vector<Value>& args) {
            if (args.empty() || args[0].type != Value::ARRAY) return Value::None();
            reverse(args[0].array->begin(), args[0].array->end());
            return Value::None();
        };

        nativeFuncs["sort"] = [](vector<Value>& args) {
            if (args.empty() || args[0].type != Value::ARRAY) return Value::None();
            
            sort(args[0].array->begin(), args[0].array->end(), 
                [](const Value& a, const Value& b) {
                    return a.toNumber() < b.toNumber();
                });
            
            return Value::None();
        };

        // ==================== TYPE CONVERSION ====================
        nativeFuncs["num"] = [](vector<Value>& args) {
            if (args.empty()) return Value::Number(0);
            return Value::Number(args[0].toNumber());
        };

        nativeFuncs["toNumber"] = [](vector<Value>& args) {
            if (args.empty()) return Value::Number(0);
            return Value::Number(args[0].toNumber());
        };

        nativeFuncs["str"] = [](vector<Value>& args) {
            if (args.empty()) return Value::String("");
            return Value::String(args[0].toString());
        };

        nativeFuncs["toString"] = [](vector<Value>& args) {
            if (args.empty()) return Value::String("");
            return Value::String(args[0].toString());
        };

        nativeFuncs["toBool"] = [](vector<Value>& args) {
            if (args.empty()) return Value::Bool(false);
            return Value::Bool(args[0].toBool());
        };

        nativeFuncs["type"] = [](vector<Value>& args) {
            if (args.empty()) return Value::String("none");
            
            switch (args[0].type) {
                case Value::NUM: return Value::String("num");
                case Value::STR: return Value::String("str");
                case Value::BOOL: return Value::String("bool");
                case Value::ARRAY: return Value::String("array");
                case Value::FILE: return Value::String("file");
                case Value::FUNC_REF: return Value::String("function");
                default: return Value::String("none");
            }
        };

        // ==================== UTILITY ====================
        nativeFuncs["sleep"] = [](vector<Value>& args) -> Value {
            if (args.empty()) return Value::None();
            int ms = (int)args[0].toNumber();
            std::this_thread::sleep_for(std::chrono::milliseconds(ms));
            return Value::None();
        };

        nativeFuncs["clock"] = [](vector<Value>& args) -> Value {
            auto now = std::chrono::system_clock::now();
            auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(now.time_since_epoch()).count();
            return Value::Number((double)ms);
        };

        // ==================== ARRAY CREATION ====================
        nativeFuncs["array"] = [](vector<Value>& args) {
            if (args.empty()) return Value::Array();
            int size = (int)args[0].toNumber();
            Value fillValue = args.size() > 1 ? args[1] : Value::Number(0);
            vector<Value> arr(size, fillValue);
            return Value::Array(arr);
        };

        nativeFuncs["range"] = [](vector<Value>& args) {
            if (args.empty()) return Value::Array();
            int start = 0, end = 0, step = 1;
            
            if (args.size() == 1) {
                end = (int)args[0].toNumber();
            } else if (args.size() == 2) {
                start = (int)args[0].toNumber();
                end = (int)args[1].toNumber();
            } else if (args.size() >= 3) {
                start = (int)args[0].toNumber();
                end = (int)args[1].toNumber();
                step = (int)args[2].toNumber();
            }
            
            vector<Value> arr;
            if (step > 0) {
                for (int i = start; i < end; i += step) {
                    arr.push_back(Value::Number(i));
                }
            } else if (step < 0) {
                for (int i = start; i > end; i += step) {
                    arr.push_back(Value::Number(i));
                }
            }
            
            return Value::Array(arr);
        };
    }

    void run(vector<shared_ptr<ASTNode>> statements) {
        for (auto& stmt : statements) {
            eval(stmt);
        }
    }
};

// ===================== MAIN =====================
int main(int argc, char* argv[]) {
    try {
        if (argc < 2) {
            cout << "EZ Language Interpreter v3.0\n";
            cout << "Usage: " << argv[0] << " <filename.ez>\n";
            return 0;
        }

        ifstream file(argv[1]);
        if (!file) {
            cerr << "Error: Cannot open file '" << argv[1] << "'\n";
            return 1;
        }

        stringstream buffer;
        buffer << file.rdbuf();
        string code = buffer.str();

        auto tokens = tokenize(code);
        Parser parser(tokens);
        auto ast = parser.parse();
        
        Interpreter interpreter;
        interpreter.run(ast);

    } catch (InterpreterException& e) {
        cerr << e.what() << endl;
        return 1;
    } catch (exception& e) {
        cerr << "Fatal error: " << e.what() << endl;
        return 1;
    }

    return 0;
}