#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ---------- Token & Symbol Definitions ---------- */

typedef struct {
    char token[32];
    char lexeme[256];
    int  line;
} Tok;

#define MAXTOK 100000
static Tok toks[MAXTOK];
static int ntok = 0;
static int pos  = 0;

typedef struct {
    char lexeme[128];
    char type[32];
    char scope[64];
    int  array_size;
} Sym;

#define MAXSYM 4096
static Sym symtab[MAXSYM];
static int nsym = 0;

static char cur_scope[64] = "Global";
static int error_count = 0;

/* internal type codes */
#define TYPE_ERROR 0
#define TYPE_INT   1
#define TYPE_CHAR  2

/* ---------- Utility / Error Functions ---------- */

static void semantic_error(const char *msg, int line) {
    fprintf(stderr, "Line %d: %s\n", line, msg);
    error_count++;
}

/* syntax error helper (we still keep minimal syntax checks) */
static void syn_error(const char *msg) {
    Tok t = (pos < ntok) ? toks[pos] : (Tok){"EOF", "", 999999};
    fprintf(stderr, "Line %d: %s\n", t.line, msg);
    error_count++;
    int cur = t.line;
    while (pos < ntok && toks[pos].line == cur) pos++;
}

/* ---------- Token Helpers ---------- */

static Tok LA(void) {
    if (pos < ntok) return toks[pos];
    Tok eof = {"EOF", "", 999999};
    return eof;
}

static Tok consume(void) {
    if (pos < ntok) return toks[pos++];
    Tok eof = {"EOF", "", 999999};
    return eof;
}

static int match(const char *tk, Tok *out) {
    Tok a = LA();
    if (strcmp(a.token, tk) == 0) {
        if (out) *out = a;
        consume();
        return 1;
    }
    return 0;
}

/* ---------- Symbol Table / Type Helpers ---------- */

static Sym* lookup_symbol(const char *name) {
    /* current scope first */
    for (int i = 0; i < nsym; ++i) {
        if (strcmp(symtab[i].lexeme, name) == 0 &&
            strcmp(symtab[i].scope,  cur_scope) == 0)
            return &symtab[i];
    }
    /* then Global */
    for (int i = 0; i < nsym; ++i) {
        if (strcmp(symtab[i].lexeme, name) == 0 &&
            strcmp(symtab[i].scope,  "Global") == 0)
            return &symtab[i];
    }
    return NULL;
}

static int str_to_type(const char *t) {
    if (strcmp(t, "Int")  == 0)  return TYPE_INT;
    if (strcmp(t, "Char") == 0)  return TYPE_CHAR;
    return TYPE_ERROR;
}

static int const_token_type(const Tok *t) {
    if (strcmp(t->token, "INT_CONST")  == 0) return TYPE_INT;
    if (strcmp(t->token, "CHAR_CONST") == 0) return TYPE_CHAR;
    return TYPE_ERROR;
}

static void add_symbol(const char *name, const char *type, const char *scope, int arrsz) {
    if (nsym >= MAXSYM) return;

    /* Multiple declarations in same scope */
    for (int i = 0; i < nsym; ++i) {
        if (strcmp(symtab[i].lexeme, name) == 0 &&
            strcmp(symtab[i].scope,  scope) == 0) {
            int line = (pos > 0) ? toks[pos-1].line : 0;
            semantic_error("Multiple declarations of same identifier.", line);
            return;
        }
    }

    snprintf(symtab[nsym].lexeme, sizeof(symtab[nsym].lexeme), "%s", name);
    snprintf(symtab[nsym].type,   sizeof(symtab[nsym].type),   "%s", type);
    snprintf(symtab[nsym].scope,  sizeof(symtab[nsym].scope),  "%s", scope);
    symtab[nsym].array_size = arrsz;
    nsym++;
}

/* ---------- Grammar Prototypes ---------- */

static void program(void);
static int  type_specifier(char *out);
static void global_decl_list(void);
static void declaration(const char *typestr);
static void init_declarator_list(const char *typestr);
static void init_declarator(const char *typestr);
static int  array_opt(int *size_out);
static int  init_opt(void);
static void function_def(const char *ret_type);
static void stmt_list_opt(void);
static void statement(void);
static void block(void);
static void expr_stmt(void);
static void if_stmt(void);
static void while_stmt(void);
static void for_stmt(void);
static int  expression_if_any(int *out_type);
static int  is_operator(const char *tk);

/* ---------- Small Helpers ---------- */

static int is_type_token(const char *tk) {
    return strcmp(tk,"VOID")==0 || strcmp(tk,"CHAR")==0 || strcmp(tk,"INT")==0;
}

static const char* norm_type_token(const char *tk) {
    if (strcmp(tk,"VOID")==0) return "Void";
    if (strcmp(tk,"CHAR")==0) return "Char";
    if (strcmp(tk,"INT")==0)  return "Int";
    return "?";
}

/* ---------- Grammar Implementation (with semantics) ---------- */

/* program: global_decl_list function_def */
static void program(void) {
    global_decl_list();
    char ftype[16];
    if (!type_specifier(ftype)) { syn_error("Any keyword expected"); return; }
    function_def(ftype);
}

/* global_decl_list: { type_specifier (NOT MAIN) declaration } */
static void global_decl_list(void) {
    for (;;) {
        Tok t = LA();
        if (!is_type_token(t.token)) return;

        /* lookahead to see if this is function_def */
        consume();
        Tok t2 = LA();
        pos--;
        if (strcmp(t2.token, "MAIN") == 0) {
            return; /* function_def starts here */
        }

        char typestr[16];
        if (!type_specifier(typestr)) { syn_error("Any keyword expected"); return; }
        declaration(typestr);
    }
}

/* type_specifier: VOID | CHAR | INT */
static int type_specifier(char *out) {
    Tok t = LA();
    if (!is_type_token(t.token)) return 0;
    strcpy(out, norm_type_token(t.token));
    consume();
    return 1;
}

/* declaration: type_specifier init_declarator_list ';' */
static void declaration(const char *typestr) {
    init_declarator_list(typestr);
    if (!match("SEMICOLON", NULL)) syn_error("Semicolon expected");
}

/* init_declarator_list: init_declarator { ',' init_declarator } */
static void init_declarator_list(const char *typestr) {
    init_declarator(typestr);
    while (match("COMMA", NULL)) {
        init_declarator(typestr);
    }
}

/* init_declarator: IDENTIFIER array_opt init_opt */
static void init_declarator(const char *typestr) {
    Tok id;
    if (!match("IDENTIFIER", &id)) { syn_error("Identifier expected"); return; }

    int arrsz = 0;
    array_opt(&arrsz);
    init_opt();
    add_symbol(id.lexeme, typestr, cur_scope, arrsz);
}

/* array_opt: empty | '[' INT_CONST ']' */
static int array_opt(int *size_out) {
    if (!match("LBRACKET", NULL)) return 0;
    int size = 0;
    Tok num;
    if (match("INT_CONST", &num)) size = atoi(num.lexeme);
    if (!match("RBRACKET", NULL)) syn_error("Right bracket expected");
    if (size_out) *size_out = size;
    return 1;
}

/* init_opt: empty | '=' (INT_CONST | CHAR_CONST) */
static int init_opt(void) {
    if (!match("ASSIGN", NULL)) return 0;
    Tok t = LA();
    if (strcmp(t.token,"INT_CONST")==0 || strcmp(t.token,"CHAR_CONST")==0) {
        consume();
        return 1;
    }
    syn_error("Identifier or integer constant expected");
    return 1;
}

/* function_def: type MAIN '(' params ')' '{' stmt_list_opt '}' */
static void function_def(const char *ret_type) {
    (void)ret_type; /* not used in this phase */

    if (!match("MAIN", NULL)) { syn_error("MAIN expected"); return; }
    if (!match("LPAREN", NULL)) syn_error("Opening parenthesis missing");

    /* parameters: either VOID, or (type IDENTIFIER {, type IDENTIFIER}) */
    Tok t = LA();
    if (strcmp(t.token,"VOID")==0) {
        consume();
    } else {
        for (;;) {
            char pty[16];
            if (!type_specifier(pty)) syn_error("Any keyword expected");
            Tok pid;
            if (!match("IDENTIFIER", &pid)) syn_error("Identifier expected");
            add_symbol(pid.lexeme, pty, "Main", 0);
            if (!match("COMMA", NULL)) break;
        }
    }

    if (!match("RPAREN", NULL)) syn_error("Closing parenthesis missing");
    if (!match("LBRACE", NULL)) syn_error("{ missing");

    strcpy(cur_scope, "Main");
    stmt_list_opt();
    if (!match("RBRACE", NULL)) syn_error("} missing");
    strcpy(cur_scope, "Global");
}

/* stmt_list_opt: { statement } */
static void stmt_list_opt(void) {
    for (;;) {
        Tok t = LA();
        if (strcmp(t.token,"RBRACE")==0 || strcmp(t.token,"EOF")==0) return;
        statement();
    }
}

/* statement: declaration | expr_stmt | if_stmt | while_stmt | for_stmt | block */
static void statement(void) {
    Tok t = LA();
    if (is_type_token(t.token)) {
        char typestr[16];
        if (!type_specifier(typestr)) { syn_error("Any keyword expected"); return; }
        declaration(typestr);
    } else if (strcmp(t.token,"IF")==0) {
        if_stmt();
    } else if (strcmp(t.token,"WHILE")==0) {
        while_stmt();
    } else if (strcmp(t.token,"FOR")==0) {
        for_stmt();
    } else if (strcmp(t.token,"LBRACE")==0) {
        block();
    } else {
        expr_stmt();
    }
}

/* block: '{' stmt_list_opt '}' */
static void block(void) {
    if (!match("LBRACE", NULL)) { syn_error("{ missing"); return; }
    stmt_list_opt();
    if (!match("RBRACE", NULL)) syn_error("} missing");
}

/* expr_stmt: expression ';' | ';' */
static void expr_stmt(void) {
    if (match("SEMICOLON", NULL)) return;
    int expr_type = TYPE_ERROR;
    if (!expression_if_any(&expr_type))
        syn_error("Identifier or integer constant expected");
    if (!match("SEMICOLON", NULL)) syn_error("Semicolon expected");
}

/* if_stmt: IF '(' expression ')' block [ ELSE block ] */
static void if_stmt(void) {
    if (!match("IF", NULL)) { syn_error("IF expected"); return; }
    if (!match("LPAREN", NULL)) syn_error("Opening parenthesis missing");

    int cond_type = TYPE_ERROR;
    if (!expression_if_any(&cond_type))
        syn_error("Identifier or integer constant expected");
    else if (cond_type != TYPE_INT && cond_type != TYPE_ERROR) {
        Tok t = LA();
        semantic_error("Integer expected in conditional expression.", t.line);
    }

    if (!match("RPAREN", NULL)) syn_error("Closing parenthesis missing");
    block();
    if (match("ELSE", NULL)) block();
}

/* while_stmt: WHILE '(' expression ')' block */
static void while_stmt(void) {
    if (!match("WHILE", NULL)) { syn_error("WHILE expected"); return; }
    if (!match("LPAREN", NULL)) syn_error("Opening parenthesis missing");

    int cond_type = TYPE_ERROR;
    if (!expression_if_any(&cond_type))
        syn_error("Identifier or integer constant expected");
    else if (cond_type != TYPE_INT && cond_type != TYPE_ERROR) {
        Tok t = LA();
        semantic_error("Integer expected in conditional expression.", t.line);
    }

    if (!match("RPAREN", NULL)) syn_error("Closing parenthesis missing");
    block();
}

/* for_stmt: FOR '(' expression ';' expression ';' expression ')' statement */
static void for_stmt(void) {
    if (!match("FOR", NULL)) { syn_error("FOR expected"); return; }
    if (!match("LPAREN", NULL)) syn_error("Opening parenthesis missing");

    /* first expression (init) */
    int e1_type = TYPE_ERROR;
    if (!expression_if_any(&e1_type))
        syn_error("Identifier or integer constant expected");
    if (!match("SEMICOLON", NULL)) syn_error("Semicolon expected");

    /* second expression (condition) must be int */
    int cond_type = TYPE_ERROR;
    if (!expression_if_any(&cond_type))
        syn_error("Identifier or integer constant expected");
    else if (cond_type != TYPE_INT && cond_type != TYPE_ERROR) {
        Tok t = LA();
        semantic_error("Integer expected in conditional expression.", t.line);
    }
    if (!match("SEMICOLON", NULL)) syn_error("Semicolon expected");

    /* third expression (increment) */
    int e3_type = TYPE_ERROR;
    if (!expression_if_any(&e3_type))
        syn_error("Identifier or integer constant expected");
    if (!match("RPAREN", NULL)) syn_error("Closing parenthesis missing");

    statement();
}

/* ---------- Expression + Type Rules ---------- */

static int is_operator(const char *tk) {
    return strcmp(tk,"PLUS")==0  || strcmp(tk,"MINUS")==0 ||
           strcmp(tk,"STAR")==0  || strcmp(tk,"SLASH")==0 ||
           strcmp(tk,"GT")==0    || strcmp(tk,"LT")==0    ||
           strcmp(tk,"EQ")==0    || strcmp(tk,"ASSIGN")==0;
}

static int apply_binary_op(const char *op, int lhs_type, int rhs_type, int line) {
    /* assignment: lhs and rhs must match */
    if (strcmp(op,"ASSIGN")==0) {
        if (lhs_type == TYPE_ERROR || rhs_type == TYPE_ERROR) return TYPE_ERROR;
        if (lhs_type != rhs_type) {
            semantic_error("Type mismatch in statement or expression.", line);
            return TYPE_ERROR;
        }
        return lhs_type;
    }

    /* arithmetic: + - * / */
    if (strcmp(op,"PLUS")==0 || strcmp(op,"MINUS")==0 ||
        strcmp(op,"STAR")==0 || strcmp(op,"SLASH")==0) {
        if (lhs_type == TYPE_ERROR || rhs_type == TYPE_ERROR) return TYPE_ERROR;
        if (lhs_type != rhs_type) {
            semantic_error("Type mismatch in statement or expression.", line);
            return TYPE_ERROR;
        }
        /* int+int -> int, char+char -> char */
        return lhs_type;
    }

    /* relational / equality: result is int, both sides same type */
    if (strcmp(op,"LT")==0 || strcmp(op,"GT")==0 || strcmp(op,"EQ")==0) {
        if (lhs_type == TYPE_ERROR || rhs_type == TYPE_ERROR) return TYPE_ERROR;
        if (lhs_type != rhs_type) {
            semantic_error("Type mismatch in statement or expression.", line);
            return TYPE_ERROR;
        }
        return TYPE_INT;
    }

    return TYPE_ERROR;
}

/* expression: (IDENTIFIER|INT_CONST|CHAR_CONST) { op (IDENTIFIER|INT_CONST|CHAR_CONST) } */
static int expression_if_any(int *out_type) {
    Tok a = LA();
    int cur_type;

    if (strcmp(a.token,"IDENTIFIER")==0) {
        Sym *s = lookup_symbol(a.lexeme);
        if (!s) {
            semantic_error("Undeclared identifier.", a.line);
            cur_type = TYPE_ERROR;
        } else {
            cur_type = str_to_type(s->type);
        }
        consume();
    } else if (strcmp(a.token,"INT_CONST")==0 || strcmp(a.token,"CHAR_CONST")==0) {
        cur_type = const_token_type(&a);
        consume();
    } else {
        return 0; /* no expression */
    }

    for (;;) {
        Tok op = LA();
        if (!is_operator(op.token)) break;
        consume();

        Tok b = LA();
        int rhs_type;
        if (strcmp(b.token,"IDENTIFIER")==0) {
            Sym *s = lookup_symbol(b.lexeme);
            if (!s) {
                semantic_error("Undeclared identifier.", b.line);
                rhs_type = TYPE_ERROR;
            } else {
                rhs_type = str_to_type(s->type);
            }
            consume();
        } else if (strcmp(b.token,"INT_CONST")==0 || strcmp(b.token,"CHAR_CONST")==0) {
            rhs_type = const_token_type(&b);
            consume();
        } else {
            syn_error("Identifier or integer constant expected");
            break;
        }

        cur_type = apply_binary_op(op.token, cur_type, rhs_type, op.line);
    }

    if (out_type) *out_type = cur_type;
    return 1;
}

/* ---------- I/O: Load Tokens, Dump Symbol Table ---------- */
static int load_tokens(const char *fname) {
    FILE *f = fopen(fname, "r");
    if (!f) {
        fprintf(stderr, "Cannot open %s\n", fname);
        return 0;
    }

    ntok = 0;

    char t1[64], t2[256];
    int line;

    while (!feof(f)) {

        int count = fscanf(f, "%63s %255s %d", t1, t2, &line);

        if (count == EOF || count == 0)
            break;

        /* If the third value wasn't an integer → SKIP THAT LINE (header or garbage) */
        if (count != 3) {
            /* discard the rest of the line */
            int c;
            while ((c = fgetc(f)) != '\n' && c != EOF);
            continue;
        }

        /* VALID TOKEN — store it */
        strcpy(toks[ntok].token,  t1);
        strcpy(toks[ntok].lexeme, t2);
        toks[ntok].line = line;

        ntok++;
        if (ntok >= MAXTOK) break;
    }

    fclose(f);
    return 1;
}


static void print_symbol_table(void) {
    FILE *out = fopen("symbol_table_semantic.txt", "w");
    if (!out) {
        fprintf(stderr, "Failed to create symbol_table_semantic.txt\n");
        return;
    }

    fprintf(out, "Lexeme\tType\tScope\tArray size\n");
    for (int i = 0; i < nsym; i++) {
        fprintf(out,
                "%s\t%s\t%s\t%d\n",
                symtab[i].lexeme,
                symtab[i].type,
                symtab[i].scope,
                symtab[i].array_size);
    }

    fclose(out);
}

/* ---------- main ---------- */

int main(void) {
    if (!load_tokens("tokens.txt")) return 1;

    program();
    print_symbol_table();

    if (error_count == 0) {
        printf("Semantic analysis finished with no errors.\n");
    } else {
        printf("Semantic analysis finished with %d error(s).\n", error_count);
    }

    return 0;
}
