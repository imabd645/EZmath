#include <iostream>
#include <vector>
#include <string>
#include <unordered_map>
#include <sstream>
#include <fstream>
#include <cctype>
#include <memory>
#include <limits>
#include <functional>
#include <chrono>
#include <thread>

using namespace std;

// ===================== TOKEN =====================
enum TokenType {
    NUMBER, STRING, IDENT, BOOL,
    PLUS, MINUS, MUL, DIV, MOD,
    ASSIGN, LPAREN, RPAREN,
    LBRACE, RBRACE, LBRACKET, RBRACKET, COMMA,
    IF, ELSE, FOR, TO, UNTIL,
    FUNC, RETURN, PRINT, INPUT,
    EQ, LT, LTE, GT, GTE, NEQ,
    AND, OR, NOT,
    BREAK, CONTINUE,
    INC, DEC,
    PLUSEQ, MINUSEQ, MULEQ, DIVEQ, MODEQ,
    GET,
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
    InterpreterException(const string &message, int line) {
        msg = "Error on line " + to_string(line) + ": " + message;
    }
    const char* what() const noexcept override {
        return msg.c_str();
    }
};

void error(const string &msg, int line) {
    throw InterpreterException(msg, line);
}

// ===================== FILE =====================
string readFile(const string &name) {
    ifstream f(name);
    if (!f) {
        cerr << "Error: File not found: " << name << endl;
        exit(1);
    }
    stringstream ss;
    ss << f.rdbuf();
    return ss.str();
}

// ===================== LEXER =====================
vector<Token> tokenize(const string &src) {
    vector<Token> tokens;
    int line = 1;

    for (size_t i = 0; i < src.size();) {
        if (src[i] == '\n') { line++; i++; continue; }
        if (isspace(src[i])) { i++; continue; }

        // Comment
        if (src[i] == '/' && i+1<src.size() && src[i+1] == '/') {
            while (i<src.size() && src[i] != '\n') i++;
            continue;
        }

        // Number (including negative numbers)
        if (isdigit(src[i]) || (src[i] == '-' && i+1 < src.size() && isdigit(src[i+1]))) {
            string n;
            bool hasDot = false;
            if (src[i] == '-') n += src[i++];
            while (i<src.size() && (isdigit(src[i]) || src[i]=='.')) {
                if (src[i] == '.') {
                    if (hasDot) error("Invalid number format", line);
                    hasDot = true;
                }
                n+=src[i++];
            }
            tokens.push_back({NUMBER,n,line});
            continue;
        }

        // String
        if (src[i] == '"') {
            i++;
            string s;
            while (i<src.size() && src[i]!='"') {
                if (src[i] == '\\' && i+1 < src.size()) {
                    i++;
                    if (src[i] == 'n') s += '\n';
                    else if (src[i] == 't') s += '\t';
                    else if (src[i] == '\\') s += '\\';
                    else if (src[i] == '"') s += '"';
                    else s += src[i];
                    i++;
                } else {
                    s+=src[i++];
                }
            }
            if (i >= src.size()) error("Unterminated string", line);
            i++;
            tokens.push_back({STRING,s,line});
            continue;
        }

        // Comparison words (check BEFORE identifiers!)
        if (i + 12 <= src.size() && src.substr(i,12)=="greaterthen"){ 
            tokens.push_back({GTE,">=",line}); i+=12; continue; 
        }
        if (i + 9 <= src.size() && src.substr(i,9)=="lessthen"){ 
            tokens.push_back({LTE,"<=",line}); i+=9; continue; 
        }
        if (i + 8 <= src.size() && src.substr(i,8)=="notequal"){ 
            tokens.push_back({NEQ,"!=",line}); i+=8; continue; 
        }
        if (i + 7 <= src.size() && src.substr(i,7)=="greater"){ 
            tokens.push_back({GT,">",line}); i+=7; continue; 
        }
        if (i + 5 <= src.size() && src.substr(i,5)=="equal"){ 
            tokens.push_back({EQ,"==",line}); i+=5; continue; 
        }
        if (i + 4 <= src.size() && src.substr(i,4)=="less"){ 
            tokens.push_back({LT,"<",line}); i+=4; continue; 
        }

        // Identifiers / Keywords
        if (isalpha(src[i])) {
            string id;
            while (i<src.size() && (isalnum(src[i]) || src[i]=='_')) id += src[i++];

            if (id=="when") tokens.push_back({IF,id,line});
            else if (id=="other") tokens.push_back({ELSE,id,line});
            else if (id=="repeat") tokens.push_back({FOR,id,line});
            else if (id=="to") tokens.push_back({TO,id,line});
            else if (id=="until") tokens.push_back({UNTIL,id,line});
            else if (id=="task") tokens.push_back({FUNC,id,line});
            else if (id=="give") tokens.push_back({RETURN,id,line});
            else if (id=="out") tokens.push_back({PRINT,id,line});
            else if (id=="in") tokens.push_back({INPUT,id,line});
            else if (id=="yes") tokens.push_back({BOOL,"1",line});
            else if (id=="no") tokens.push_back({BOOL,"0",line});
            else if (id=="escape") tokens.push_back({BREAK,id,line});
            else if (id=="skip") tokens.push_back({CONTINUE,id,line});
            else if (id=="and") tokens.push_back({AND,id,line});
            else if (id=="or") tokens.push_back({OR,id,line});
            else if (id=="not") tokens.push_back({NOT,id,line});
            else if (id=="get") tokens.push_back({GET,id,line});
            else tokens.push_back({IDENT,id,line});
            continue;
        }

        // Operators
        if (src[i]=='+'){ 
            if (i+1<src.size() && src[i+1]=='+') { tokens.push_back({INC,"++",line}); i+=2; continue; }
            if (i+1<src.size() && src[i+1]=='=') { tokens.push_back({PLUSEQ,"+=",line}); i+=2; continue; }
            tokens.push_back({PLUS,"+",line}); i++; continue; 
        }
        if (src[i]=='-'){ 
            if (i+1<src.size() && src[i+1]=='-') { tokens.push_back({DEC,"--",line}); i+=2; continue; }
            if (i+1<src.size() && src[i+1]=='=') { tokens.push_back({MINUSEQ,"-=",line}); i+=2; continue; }
            tokens.push_back({MINUS,"-",line}); i++; continue; 
        }
        if (src[i]=='*'){ 
            if (i+1<src.size() && src[i+1]=='=') { tokens.push_back({MULEQ,"*=",line}); i+=2; continue; }
            tokens.push_back({MUL,"*",line}); i++; continue; 
        }
        if (src[i]=='/'){ 
            if (i+1<src.size() && src[i+1]=='=') { tokens.push_back({DIVEQ,"/=",line}); i+=2; continue; }
            tokens.push_back({DIV,"/",line}); i++; continue; 
        }
        if (src[i]=='%'){ 
            if (i+1<src.size() && src[i+1]=='=') { tokens.push_back({MODEQ,"%=",line}); i+=2; continue; }
            tokens.push_back({MOD,"%",line}); i++; continue; 
        }
        if (src[i]=='='){
            if (i+1<src.size() && src[i+1]=='='){ tokens.push_back({EQ,"==",line}); i+=2; continue; }
            tokens.push_back({ASSIGN,"=",line}); i++; continue;
        }
        if (src[i]=='!'){ 
            if (i+1<src.size() && src[i+1]=='='){ tokens.push_back({NEQ,"!=",line}); i+=2; continue; }
            error("Unexpected '!' character", line);
        }
        if (src[i]=='<'){
            if (i+1<src.size() && src[i+1]=='='){ tokens.push_back({LTE,"<=",line}); i+=2; continue; }
            tokens.push_back({LT,"<",line}); i++; continue;
        }
        if (src[i]=='>'){
            if (i+1<src.size() && src[i+1]=='='){ tokens.push_back({GTE,">=",line}); i+=2; continue; }
            tokens.push_back({GT,">",line}); i++; continue;
        }
        if (src[i]=='('){ tokens.push_back({LPAREN,"(",line}); i++; continue; }
        if (src[i]==')'){ tokens.push_back({RPAREN,")",line}); i++; continue; }
        if (src[i]=='{'){ tokens.push_back({LBRACE,"{",line}); i++; continue; }
        if (src[i]=='}'){ tokens.push_back({RBRACE,"}",line}); i++; continue; }
        if (src[i]=='['){ tokens.push_back({LBRACKET,"[",line}); i++; continue; }
        if (src[i]==']'){ tokens.push_back({RBRACKET,"]",line}); i++; continue; }
        if (src[i]==','){ tokens.push_back({COMMA,",",line}); i++; continue; }

        error("Unknown character: "+string(1,src[i]),line);
    }

    tokens.push_back({END,"",line});
    return tokens;
}

// ===================== VALUE =====================
struct Value {
    enum Type { NUM, STR, BOOL, ARR } type;
    double num = 0;
    string str = "";
    bool boolean = false;
    vector<Value> arr;

    Value() : type(NUM), num(0) {}
    
    static Value Number(double v) { 
        Value val; val.type = NUM; val.num = v; return val;
    }
    static Value String(const string &s) { 
        Value val; val.type = STR; val.str = s; return val;
    }
    static Value Bool(bool b) { 
        Value val; val.type = BOOL; val.boolean = b; return val;
    }
    static Value Array(const vector<Value> &a) { 
        Value val; val.type = ARR; val.arr = a; return val;
    }
    
    bool toBool() const {
        if (type == BOOL) return boolean;
        if (type == NUM) return num != 0;
        if (type == STR) return !str.empty();
        if (type == ARR) return !arr.empty();
        return false;
    }
    
    double toNumber() const {
        if (type == NUM) return num;
        if (type == BOOL) return boolean ? 1.0 : 0.0;
        if (type == STR) {
            try {
                return stod(str);
            } catch (...) {
                return 0.0;
            }
        }
        return 0.0;
    }
    
    string toString() const {
        if (type == STR) return str;
        if (type == NUM) {
            if (num == (int)num) {
                return to_string((int)num);
            }
            string s = to_string(num);
            s.erase(s.find_last_not_of('0') + 1, string::npos);
            if (s.back() == '.') s.pop_back();
            return s;
        }
        if (type == BOOL) return boolean ? "yes" : "no";
        if (type == ARR) {
            string s = "[";
            for (size_t i = 0; i < arr.size(); i++) {
                s += arr[i].toString();
                if (i < arr.size() - 1) s += ", ";
            }
            s += "]";
            return s;
        }
        return "";
    }

    bool equals(const Value &other) const {
        if (type != other.type) {
            return toString() == other.toString();
        }
        if (type == NUM) return num == other.num;
        if (type == STR) return str == other.str;
        if (type == BOOL) return boolean == other.boolean;
        return false;
    }
};

// ===================== FUNCTION =====================
struct Function {
    vector<string> params;
    size_t bodyStart;
};

// ===================== CONTROL FLOW =====================
enum ControlFlow { NONE, BREAK_FLAG, CONTINUE_FLAG, RETURN_FLAG };

// ===================== INTERPRETER =====================
class EZ {
    vector<Token> t;
    size_t p = 0;
    vector<unordered_map<string, Value>> scopes;
    unordered_map<string, Function> functions;
    unordered_map<string, function<Value(vector<Value>&, int)>> builtins;
    ControlFlow flow = NONE;
    Value returnValue;
    int recursionDepth = 0;
    static const int MAX_RECURSION = 1000;

    Token cur() { 
        if (p >= t.size()) return t[t.size()-1];
        return t[p]; 
    }
    
    void next() { if (p < t.size()) p++; }
    
    Token peek(int offset = 1) { 
        if (p + offset < t.size()) return t[p + offset];
        return t[t.size() - 1];
    }

    void pushScope() { scopes.push_back({}); }
    void popScope() { if (!scopes.empty()) scopes.pop_back(); }

    void setVar(const string &name, const Value &val) {
        for (int i = scopes.size() - 1; i >= 0; i--) {
            if (scopes[i].count(name)) {
                scopes[i][name] = val;
                return;
            }
        }
        scopes.back()[name] = val;
    }

    Value getVar(const string &name) {
        for (int i = scopes.size() - 1; i >= 0; i--) {
            if (scopes[i].count(name)) return scopes[i][name];
        }
        error("Undefined variable: " + name, cur().line);
        return Value::Number(0);
    }

    bool varExists(const string &name) {
        for (int i = scopes.size() - 1; i >= 0; i--) {
            if (scopes[i].count(name)) return true;
        }
        return false;
    }

    void initBuiltins() {
        // len(arr) - get array length
        builtins["len"] = [](vector<Value> &args, int line) -> Value {
            if (args.size() != 1) error("len expects 1 argument", line);
            if (args[0].type == Value::ARR) {
                return Value::Number(args[0].arr.size());
            } else if (args[0].type == Value::STR) {
                return Value::Number(args[0].str.length());
            }
            error("len expects array or string", line);
            return Value::Number(0);
        };

        // add(arr, value) - add to array
        builtins["add"] = [this](vector<Value> &args, int line) -> Value {
            if (args.size() != 2) error("add expects 2 arguments (array, value)", line);
            if (args[0].type != Value::ARR) error("First argument to add must be array", line);
            args[0].arr.push_back(args[1]);
            return args[0];
        };

        // remove(arr) - remove last element from array and return modified array
        builtins["remove"] = [](vector<Value> &args, int line) -> Value {
            if (args.size() != 1) error("remove expects 1 argument", line);
            if (args[0].type != Value::ARR) error("remove expects array", line);
            if (args[0].arr.empty()) error("Cannot remove from empty array", line);
            Value result = args[0];
            result.arr.pop_back();
            return result;
        };

        // clock() - get current time in milliseconds
        builtins["clock"] = [](vector<Value> &args, int line) -> Value {
            if (args.size() != 0) error("clock expects no arguments", line);
            auto now = chrono::system_clock::now();
            auto duration = now.time_since_epoch();
            auto millis = chrono::duration_cast<chrono::milliseconds>(duration).count();
            return Value::Number(static_cast<double>(millis));
        };

        // stop(milliseconds) - sleep for given milliseconds
        builtins["stop"] = [](vector<Value> &args, int line) -> Value {
            if (args.size() != 1) error("stop expects 1 argument (milliseconds)", line);
            int ms = static_cast<int>(args[0].toNumber());
            if (ms < 0) error("stop duration must be non-negative", line);
            this_thread::sleep_for(chrono::milliseconds(ms));
            return Value::Number(0);
        };
    }

    Value factor() {
        if (cur().type == LPAREN) {
            next();
            Value v = logicalOr();
            if (cur().type != RPAREN) error("Expected ')'", cur().line);
            next();
            return v;
        }

        if (cur().type == NOT) {
            next();
            Value v = factor();
            return Value::Bool(!v.toBool());
        }

        if (cur().type == NUMBER) {
            double v = stod(cur().text); 
            next(); 
            return Value::Number(v);
        }

        if (cur().type == STRING) {
            string v = cur().text; 
            next(); 
            return Value::String(v);
        }

        if (cur().type == BOOL) {
            bool v = (cur().text == "1"); 
            next(); 
            return Value::Bool(v);
        }

        if (cur().type == LBRACKET) {
            next();
            vector<Value> arr;
            while (cur().type != RBRACKET && cur().type != END) {
                arr.push_back(logicalOr());
                if (cur().type == COMMA) {
                    next();
                } else if (cur().type != RBRACKET) {
                    error("Expected ',' or ']' in array literal", cur().line);
                }
            }
            if (cur().type != RBRACKET) error("Expected ']'", cur().line);
            next();
            return Value::Array(arr);
        }

        if (cur().type == IDENT) {
            string n = cur().text;
            next();

            if (cur().type == LPAREN) {
                next();
                vector<Value> args;
                while (cur().type != RPAREN && cur().type != END) {
                    args.push_back(logicalOr());
                    if (cur().type == COMMA) next();
                }
                if (cur().type != RPAREN) error("Expected ')'", cur().line);
                next();
                return callFunction(n, args);
            }

            if (cur().type == LBRACKET) {
                next();
                Value idx = logicalOr();
                if (cur().type != RBRACKET) error("Expected ']'", cur().line);
                next();
                
                Value arr = getVar(n);
                if (arr.type != Value::ARR) error("Not an array: " + n, cur().line);
                int i = (int)idx.toNumber();
                if (i < 0 || i >= (int)arr.arr.size()) 
                    error("Array index out of bounds: " + to_string(i), cur().line);
                return arr.arr[i];
            }

            return getVar(n);
        }

        error("Unexpected token: " + cur().text, cur().line);
        return Value::Number(0);
    }

    Value term() {
        Value v = factor();
        while (cur().type == MUL || cur().type == DIV || cur().type == MOD) {
            TokenType op = cur().type;
            int line = cur().line;
            next();
            Value r = factor();
            
            if (op == MUL) {
                v.num = v.toNumber() * r.toNumber();
                v.type = Value::NUM;
            }
            else if (op == DIV) {
                double divisor = r.toNumber();
                if (divisor == 0) error("Division by zero", line);
                v.num = v.toNumber() / divisor;
                v.type = Value::NUM;
            }
            else {
                int divisor = (int)r.toNumber();
                if (divisor == 0) error("Modulo by zero", line);
                v.num = (int)v.toNumber() % divisor;
                v.type = Value::NUM;
            }
        }
        return v;
    }

    Value expr() {
        Value v = term();
        while (cur().type == PLUS || cur().type == MINUS) {
            TokenType op = cur().type;
            next();
            Value r = term();
            
            if (op == PLUS) {
                if (v.type == Value::STR || r.type == Value::STR) {
                    v = Value::String(v.toString() + r.toString());
                } else {
                    v.num = v.toNumber() + r.toNumber();
                    v.type = Value::NUM;
                }
            } else {
                v.num = v.toNumber() - r.toNumber();
                v.type = Value::NUM;
            }
        }
        return v;
    }

    Value comparison() {
        Value v = expr();
        
        while (cur().type == EQ || cur().type == NEQ || 
               cur().type == LT || cur().type == GT || 
               cur().type == LTE || cur().type == GTE) {
            TokenType op = cur().type;
            next();
            Value r = expr();
            
            bool result;
            if (op == EQ) {
                result = v.equals(r);
            }
            else if (op == NEQ) {
                result = !v.equals(r);
            }
            else {
                double lhs = v.toNumber();
                double rhs = r.toNumber();
                if (op == LT) result = (lhs < rhs);
                else if (op == GT) result = (lhs > rhs);
                else if (op == LTE) result = (lhs <= rhs);
                else result = (lhs >= rhs);
            }
            
            v = Value::Bool(result);
        }
        return v;
    }

    Value logicalAnd() {
        Value v = comparison();
        while (cur().type == AND) {
            next();
            if (!v.toBool()) {
                comparison();
                return Value::Bool(false);
            }
            Value r = comparison();
            v = Value::Bool(r.toBool());
        }
        return v;
    }

    Value logicalOr() {
        Value v = logicalAnd();
        while (cur().type == OR) {
            next();
            if (v.toBool()) {
                logicalAnd();
                return Value::Bool(true);
            }
            Value r = logicalAnd();
            v = Value::Bool(r.toBool());
        }
        return v;
    }

    void skipBlock() {
        if (cur().type == LBRACE) next();
        
        int depth = 1;
        while (depth > 0 && cur().type != END) {
            if (cur().type == LBRACE) depth++;
            if (cur().type == RBRACE) depth--;
            next();
        }
    }

    Value callFunction(const string &name, vector<Value> args) {
        // Check if it's a builtin function
        if (builtins.count(name)) {
            return builtins[name](args, cur().line);
        }
        
        if (!functions.count(name)) 
            error("Undefined function: " + name, cur().line);
        
        if (recursionDepth >= MAX_RECURSION)
            error("Maximum recursion depth exceeded", cur().line);
        
        Function &func = functions[name];
        if (args.size() != func.params.size()) 
            error("Function " + name + " expects " + to_string(func.params.size()) + 
                  " arguments, got " + to_string(args.size()), cur().line);
        
        size_t savedPos = p;
        ControlFlow savedFlow = flow;
        recursionDepth++;
        
        pushScope();
        for (size_t i = 0; i < args.size(); i++) {
            setVar(func.params[i], args[i]);
        }
        
        p = func.bodyStart;
        flow = NONE;
        returnValue = Value::Number(0);
        
        executeBlock();
        
        Value result = returnValue;
        
        popScope();
        recursionDepth--;
        p = savedPos;
        flow = savedFlow;
        
        return result;
    }

    void executeBlock() {
        if (cur().type != LBRACE) error("Expected '{'", cur().line);
        next();
        
        while (cur().type != RBRACE && cur().type != END && flow == NONE) {
            statement();
        }
        
        if (cur().type == RBRACE) next();
    }

    void statement() {
        if (cur().type == PRINT) {
            next();
            Value v = logicalOr();
            cout << v.toString() << endl;
            return;
        }

        if (cur().type == INPUT) {
            next();
            if (cur().type != IDENT) error("Expected variable name after 'in'", cur().line);
            string varName = cur().text;
            next();
            
            string input;
            if (!getline(cin, input)) {
                error("Failed to read input", cur().line);
            }
            
            try {
                size_t pos;
                double num = stod(input, &pos);
                if (pos == input.length()) {
                    setVar(varName, Value::Number(num));
                } else {
                    setVar(varName, Value::String(input));
                }
            } catch (...) {
                setVar(varName, Value::String(input));
            }
            return;
        }

        if (cur().type == IF) {
            next();
            Value cond = logicalOr();
            
            if (cond.toBool()) {
                executeBlock();
                // Skip all "when other" and "other" blocks
                while (cur().type == IF || cur().type == ELSE) {
                    if (cur().type == IF) {
                        next();
                        logicalOr(); // Skip condition
                    } else {
                        next();
                    }
                    skipBlock();
                }
            } else {
                skipBlock();
                // Check for "when other" (else if)
                while (cur().type == IF) {
                    next();
                    Value elseCond = logicalOr();
                    if (elseCond.toBool()) {
                        executeBlock();
                        // Skip remaining blocks
                        while (cur().type == IF || cur().type == ELSE) {
                            if (cur().type == IF) {
                                next();
                                logicalOr();
                            } else {
                                next();
                            }
                            skipBlock();
                        }
                        return;
                    } else {
                        skipBlock();
                    }
                }
                // Final "other" (else)
                if (cur().type == ELSE) {
                    next();
                    executeBlock();
                }
            }
            return;
        }

        // Foreach loop: get x in array
        if (cur().type == GET) {
            next();
            if (cur().type != IDENT) error("Expected variable name after 'get'", cur().line);
            string varName = cur().text;
            next();
            
            if (cur().type != INPUT) error("Expected 'in' after variable name in foreach loop", cur().line);
            next();
            
            if (cur().type != IDENT) error("Expected array name after 'in'", cur().line);
            string arrayName = cur().text;
            next();
            
            Value arr = getVar(arrayName);
            if (arr.type != Value::ARR) error("Expected array in foreach loop: " + arrayName, cur().line);
            
            size_t loopStart = p;
            
            for (size_t i = 0; i < arr.arr.size(); i++) {
                setVar(varName, arr.arr[i]);
                p = loopStart;
                executeBlock();
                
                if (flow == BREAK_FLAG) {
                    flow = NONE;
                    break;
                }
                if (flow == CONTINUE_FLAG) {
                    flow = NONE;
                    continue;
                }
                if (flow == RETURN_FLAG) break;
            }
            return;
        }

        if (cur().type == FOR) {
            next();
            if (cur().type != IDENT) error("Expected variable name after 'repeat'", cur().line);
            string varName = cur().text;
            next();
            
            if (cur().type != ASSIGN) error("Expected '=' in for loop", cur().line);
            next();
            Value start = logicalOr();
            
            if (cur().type != TO) error("Expected 'to' in for loop", cur().line);
            next();
            Value end = logicalOr();
            
            size_t loopStart = p;
            int startVal = (int)start.toNumber();
            int endVal = (int)end.toNumber();
            
            // Support both forward and backward loops
            if (startVal <= endVal) {
                for (int i = startVal; i <= endVal; i++) {
                    setVar(varName, Value::Number(i));
                    p = loopStart;
                    executeBlock();
                    
                    if (flow == BREAK_FLAG) {
                        flow = NONE;
                        break;
                    }
                    if (flow == CONTINUE_FLAG) {
                        flow = NONE;
                        continue;
                    }
                    if (flow == RETURN_FLAG) break;
                }
            } else {
                for (int i = startVal; i >= endVal; i--) {
                    setVar(varName, Value::Number(i));
                    p = loopStart;
                    executeBlock();
                    
                    if (flow == BREAK_FLAG) {
                        flow = NONE;
                        break;
                    }
                    if (flow == CONTINUE_FLAG) {
                        flow = NONE;
                        continue;
                    }
                    if (flow == RETURN_FLAG) break;
                }
            }
            return;
        }

        if (cur().type == BREAK) {
            flow = BREAK_FLAG;
            next();
            return;
        }

        if (cur().type == CONTINUE) {
            flow = CONTINUE_FLAG;
            next();
            return;
        }

        if (cur().type == RETURN) {
            next();
            if (cur().type != RBRACE && cur().type != END) {
                returnValue = logicalOr();
            }
            flow = RETURN_FLAG;
            return;
        }

        if (cur().type == FUNC) {
            next();
            if (cur().type != IDENT) error("Expected function name", cur().line);
            string fname = cur().text;
            next();

            if (cur().type != LPAREN) error("Expected '(' after function name", cur().line);
            next();

            vector<string> params;
            while (cur().type != RPAREN && cur().type != END) {
                if (cur().type != IDENT) error("Expected parameter name", cur().line);
                params.push_back(cur().text);
                next();
                if (cur().type == COMMA) next();
            }
            if (cur().type != RPAREN) error("Expected ')' after parameters", cur().line);
            next();

            functions[fname] = {params, p};
            skipBlock();
            return;
        }

        // Assignment and compound operators
        if (cur().type == IDENT) {
            string varName = cur().text;
            int line = cur().line;
            next();

            // Array element assignment
            if (cur().type == LBRACKET) {
                next();
                Value idx = logicalOr();
                if (cur().type != RBRACKET) error("Expected ']'", cur().line);
                next();

                if (cur().type == ASSIGN) {
                    next();
                    Value newVal = logicalOr();
                    Value arr = getVar(varName);
                    if (arr.type != Value::ARR) error("Not an array: " + varName, line);
                    int i = (int)idx.toNumber();
                    if (i < 0 || i >= (int)arr.arr.size()) 
                        error("Array index out of bounds: " + to_string(i), line);
                    arr.arr[i] = newVal;
                    setVar(varName, arr);
                    return;
                }
                error("Expected '=' after array index", cur().line);
            }

            // Regular assignment
            if (cur().type == ASSIGN) {
                next();
                Value val = logicalOr();
                setVar(varName, val);
                return;
            }

            // Compound assignments
            if (cur().type == PLUSEQ || cur().type == MINUSEQ || 
                cur().type == MULEQ || cur().type == DIVEQ || cur().type == MODEQ) {
                TokenType op = cur().type;
                next();
                Value right = logicalOr();
                Value left = getVar(varName);

                if (op == PLUSEQ) {
                    if (left.type == Value::STR || right.type == Value::STR) {
                        setVar(varName, Value::String(left.toString() + right.toString()));
                    } else {
                        setVar(varName, Value::Number(left.toNumber() + right.toNumber()));
                    }
                } else if (op == MINUSEQ) {
                    setVar(varName, Value::Number(left.toNumber() - right.toNumber()));
                } else if (op == MULEQ) {
                    setVar(varName, Value::Number(left.toNumber() * right.toNumber()));
                } else if (op == DIVEQ) {
                    double divisor = right.toNumber();
                    if (divisor == 0) error("Division by zero", line);
                    setVar(varName, Value::Number(left.toNumber() / divisor));
                } else if (op == MODEQ) {
                    int divisor = (int)right.toNumber();
                    if (divisor == 0) error("Modulo by zero", line);
                    setVar(varName, Value::Number((int)left.toNumber() % divisor));
                }
                return;
            }

            // Increment/Decrement
            if (cur().type == INC) {
                next();
                Value v = getVar(varName);
                setVar(varName, Value::Number(v.toNumber() + 1));
                return;
            }

            if (cur().type == DEC) {
                next();
                Value v = getVar(varName);
                setVar(varName, Value::Number(v.toNumber() - 1));
                return;
            }

            // If none of the above, it's a function call or standalone expression
            p--;
            logicalOr();
            return;
        }

        // Standalone expression
        logicalOr();
    }

public:
    void run(const vector<Token> &tokens) {
        t = tokens;
        p = 0;
        scopes.push_back({});
        initBuiltins();

        while (cur().type != END) {
            statement();
        }
    }
};

// ===================== MAIN =====================
int main(int argc, char* argv[]) {
    if (argc < 2) {
        cout << "Usage: " << argv[0] << " <filename.ez>" << endl;
        return 1;
    }

    try {
        string code = readFile(argv[1]);
        vector<Token> tokens = tokenize(code);
        EZ interpreter;
        interpreter.run(tokens);
    } catch (const InterpreterException &e) {
        cerr << e.what() << endl;
        return 1;
    } catch (const exception &e) {
        cerr << "Unexpected error: " << e.what() << endl;
        return 1;
    }

    return 0;
}