#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Token structure for input from lexical phase */
typedef struct {
    char token[32];
    char lexeme[256];
    int  line;
} Tok;

#define MAXTOK 100000
static Tok toks[MAXTOK];
static int ntok = 0;
static int pos  = 0;

/* Symbol table entry built during semantic analysis */
typedef struct {
    char lexeme[128];
    char type[16];
    char scope[64];
    int  array_size; /* 0 for scalar or unsized array */
} Sym;

#define MAXSYM 4096
static Sym symtab[MAXSYM];
static int nsym = 0;

static char cur_scope[64] = "Global";
static int error_count = 0;

/* Internal type codes */
#define TYPE_ERROR 0
#define TYPE_INT   1
#define TYPE_CHAR  2

/* Forward declarations of grammar functions */
static void program(void);
static int  type_specifier(char *out_type);
static void global_decl_list(void);
static void declaration(const char *typestr);
static void init_declarator_list(const char *typestr);
static void init_declarator(const char *typestr);
static int  array_opt(int *size_out);
static int  init_opt(const char *typestr, int line);
static void function_def(const char *ret_type);
static void stmt_list_opt(void);
static void statement(void);
static void block(void);
static void expr_stmt(void);
static void if_stmt(void);
static void while_stmt(void);
static void for_stmt(void);
static int  expression_if_any(int *out_type);

/* Utility: print semantic error */
static void semantic_error(const char *msg, int line) {
    fprintf(stderr, "Line %d: %s\n", line, msg);
    error_count++;
}

/* Skip all tokens with the same line number */
static void skip_line_tokens(int line) {
    while (pos < ntok && toks[pos].line == line) {
        pos++;
    }
}

/* Report syntax-style error and skip the rest of the line */
static void syn_error(const char *msg) {
    Tok t;
    if (pos < ntok) {
        t = toks[pos];
    } else {
        t.line = 0;
        strcpy(t.token, "EOF");
        t.lexeme[0] = '\0';
    }
    fprintf(stderr, "Line %d: %s\n", t.line, msg);
    error_count++;
    skip_line_tokens(t.line);
}

/* Lookahead: return current token (or EOF sentinel) */
static Tok LA(void) {
    Tok t;
    if (pos < ntok) {
        t = toks[pos];
    } else {
        strcpy(t.token, "EOF");
        t.lexeme[0] = '\0';
        t.line = 0;
    }
    return t;
}

/* Consume and return current token */
static Tok consume(void) {
    Tok t = LA();
    if (pos < ntok) {
        pos++;
    }
    return t;
}

/* Match specific token kind, optionally returning token value */
static int match(const char *kind, Tok *out) {
    Tok t = LA();
    if (strcmp(t.token, kind) == 0) {
        if (out) {
            *out = t;
        }
        consume();
        return 1;
    }
    return 0;
}

/* Check whether token is a type specifier */
static int is_type_token(const char *tk) {
    return strcmp(tk, "VOID") == 0 ||
           strcmp(tk, "CHAR") == 0 ||
           strcmp(tk, "INT")  == 0;
}

/* Normalize type tokens into symbol-table type strings */
static const char *norm_type_token(const char *tk) {
    if (strcmp(tk, "VOID") == 0) return "Void";
    if (strcmp(tk, "CHAR") == 0) return "Char";
    if (strcmp(tk, "INT")  == 0) return "Int";
    return "?";
}

/* Convert type string to internal type code */
static int str_to_type(const char *s) {
    if (strcmp(s, "Int") == 0)  return TYPE_INT;
    if (strcmp(s, "Char") == 0) return TYPE_CHAR;
    return TYPE_ERROR;
}

/* Convert constant token into internal type code */
static int const_token_type(const Tok *t) {
    if (strcmp(t->token, "INT_CONST") == 0)  return TYPE_INT;
    if (strcmp(t->token, "CHAR_CONST") == 0) return TYPE_CHAR;
    return TYPE_ERROR;
}

/* Add symbol to table, checking multiple declarations in same scope */
static void add_symbol(const char *name,
                       const char *type,
                       const char *scope,
                       int         arrsz,
                       int         line) {
    int i;

    for (i = 0; i < nsym; i++) {
        if (strcmp(symtab[i].lexeme, name) == 0 &&
            strcmp(symtab[i].scope,  scope) == 0) {
            semantic_error("Multiple declarations of same identifier.", line);
            return;
        }
    }

    if (nsym >= MAXSYM) {
        return;
    }

    snprintf(symtab[nsym].lexeme, sizeof(symtab[nsym].lexeme), "%s", name);
    snprintf(symtab[nsym].type,   sizeof(symtab[nsym].type),   "%s", type);
    snprintf(symtab[nsym].scope,  sizeof(symtab[nsym].scope),  "%s", scope);
    symtab[nsym].array_size = arrsz;
    nsym++;
}

/* Lookup identifier in current scope, then in Global scope */
static Sym *lookup_symbol(const char *name) {
    int i;

    for (i = 0; i < nsym; i++) {
        if (strcmp(symtab[i].lexeme, name) == 0 &&
            strcmp(symtab[i].scope,  cur_scope) == 0) {
            return &symtab[i];
        }
    }

    for (i = 0; i < nsym; i++) {
        if (strcmp(symtab[i].lexeme, name) == 0 &&
            strcmp(symtab[i].scope,  "Global") == 0) {
            return &symtab[i];
        }
    }

    return NULL;
}

/* Decide whether token kind is an operator */
static int is_operator(const char *tk) {
    if (strcmp(tk, "PLUS")   == 0) return 1;
    if (strcmp(tk, "MINUS")  == 0) return 1;
    if (strcmp(tk, "STAR")   == 0) return 1;
    if (strcmp(tk, "SLASH")  == 0) return 1;
    if (strcmp(tk, "GT")     == 0) return 1;
    if (strcmp(tk, "LT")     == 0) return 1;
    if (strcmp(tk, "EQ")     == 0) return 1;
    if (strcmp(tk, "ASSIGN") == 0) return 1;
    return 0;
}

/* Apply binary operator semantics and return resulting type */
static int apply_binary_op(const char *op,
                           int         lhs_type,
                           int         rhs_type,
                           int         line) {
    if (strcmp(op, "ASSIGN") == 0) {
        if (lhs_type == TYPE_ERROR || rhs_type == TYPE_ERROR) {
            return TYPE_ERROR;
        }
        if (lhs_type != rhs_type) {
            semantic_error("Type mismatch in statement or expression.", line);
            return TYPE_ERROR;
        }
        return lhs_type;
    }

    if (strcmp(op, "PLUS") == 0  ||
        strcmp(op, "MINUS") == 0 ||
        strcmp(op, "STAR") == 0  ||
        strcmp(op, "SLASH") == 0) {
        if (lhs_type == TYPE_ERROR || rhs_type == TYPE_ERROR) {
            return TYPE_ERROR;
        }
        if (lhs_type != rhs_type) {
            semantic_error("Type mismatch in statement or expression.", line);
            return TYPE_ERROR;
        }
        return lhs_type;
    }

    if (strcmp(op, "LT") == 0 ||
        strcmp(op, "GT") == 0 ||
        strcmp(op, "EQ") == 0) {
        if (lhs_type == TYPE_ERROR || rhs_type == TYPE_ERROR) {
            return TYPE_ERROR;
        }
        if (lhs_type != rhs_type) {
            semantic_error("Type mismatch in statement or expression.", line);
            return TYPE_ERROR;
        }
        return TYPE_INT;
    }

    return TYPE_ERROR;
}

/* expression_if_any parses optional expression and returns its type */
static int expression_if_any(int *out_type) {
    Tok a = LA();
    int cur_type;

    if (strcmp(a.token, "IDENTIFIER") == 0) {
        Sym *s = lookup_symbol(a.lexeme);
        if (!s) {
            semantic_error("Undeclared identifier.", a.line);
            cur_type = TYPE_ERROR;
        } else {
            cur_type = str_to_type(s->type);
        }
        consume();
    } else if (strcmp(a.token, "INT_CONST") == 0 ||
               strcmp(a.token, "CHAR_CONST") == 0) {
        cur_type = const_token_type(&a);
        consume();
    } else {
        return 0;
    }

    for (;;) {
        Tok op = LA();
        if (!is_operator(op.token)) {
            break;
        }
        consume();

        Tok b = LA();
        int rhs_type;

        if (strcmp(b.token, "IDENTIFIER") == 0) {
            Sym *s2 = lookup_symbol(b.lexeme);
            if (!s2) {
                semantic_error("Undeclared identifier.", b.line);
                rhs_type = TYPE_ERROR;
            } else {
                rhs_type = str_to_type(s2->type);
            }
            consume();
        } else if (strcmp(b.token, "INT_CONST") == 0 ||
                   strcmp(b.token, "CHAR_CONST") == 0) {
            rhs_type = const_token_type(&b);
            consume();
        } else {
            syn_error("Identifier or integer constant expected");
            break;
        }

        cur_type = apply_binary_op(op.token, cur_type, rhs_type, op.line);
    }

    if (out_type) {
        *out_type = cur_type;
    }

    return 1;
}

/* Load tokens from tokens.txt produced by lexical phase */
static int load_tokens(const char *fname) {
    FILE *f = fopen(fname, "r");
    char linebuf[512];

    if (!f) {
        fprintf(stderr, "Cannot open %s\n", fname);
        return 0;
    }

    if (!fgets(linebuf, sizeof(linebuf), f)) {
        fclose(f);
        return 0;
    }

    ntok = 0;

    while (fgets(linebuf, sizeof(linebuf), f)) {
        char t1[32];
        char t2[256];
        int  line;
        int  n = sscanf(linebuf, "%31s %255s %d", t1, t2, &line);

        if (n != 3) {
            continue;
        }
        if (ntok >= MAXTOK) {
            break;
        }

        strcpy(toks[ntok].token,  t1);
        strcpy(toks[ntok].lexeme, t2);
        toks[ntok].line = line;
        ntok++;
    }

    fclose(f);
    return 1;
}

/* Write semantic symbol table */
static void write_symbol_table(const char *fname) {
    FILE *out = fopen(fname, "w");
    int   i;

    if (!out) {
        fprintf(stderr, "Failed to create %s\n", fname);
        return;
    }

    fprintf(out, "Lexeme\tType\tScope\tArray size\n");
    for (i = 0; i < nsym; i++) {
        fprintf(out, "%s\t%s\t%s\t%d\n",
                symtab[i].lexeme,
                symtab[i].type,
                symtab[i].scope,
                symtab[i].array_size);
    }

    fclose(out);
}

/* type_specifier: VOID | CHAR | INT */
static int type_specifier(char *out_type) {
    Tok t = LA();

    if (!is_type_token(t.token)) {
        return 0;
    }

    strcpy(out_type, norm_type_token(t.token));
    consume();
    return 1;
}

/* array_opt: empty | '[' INT_CONST ']' | '[' ']' */
static int array_opt(int *size_out) {
    Tok t = LA();

    if (strcmp(t.token, "LBRACKET") != 0) {
        *size_out = 0;
        return 0;
    }

    consume();
    t = LA();

    if (strcmp(t.token, "INT_CONST") == 0) {
        int sz = atoi(t.lexeme);
        *size_out = sz;
        consume();
    } else {
        *size_out = 0;
    }

    if (!match("RBRACKET", NULL)) {
        syn_error("Right bracket expected");
    }

    return 1;
}

/* init_opt: empty | '=' constant (type checked against declaration type) */
static int init_opt(const char *typestr, int line) {
    Tok t = LA();

    if (strcmp(t.token, "ASSIGN") != 0) {
        return 0;
    }

    consume();
    t = LA();

    if (strcmp(t.token, "INT_CONST") != 0 &&
        strcmp(t.token, "CHAR_CONST") != 0) {
        syn_error("Identifier or integer constant expected");
        return 0;
    }

    {
        int ctype = const_token_type(&t);
        int dtype = str_to_type(typestr);
        if (ctype != TYPE_ERROR &&
            dtype != TYPE_ERROR &&
            ctype != dtype) {
            semantic_error("Type mismatch in statement or expression.", t.line);
        }
    }

    consume();
    return 1;
}

/* init_declarator: IDENTIFIER array_opt init_opt */
static void init_declarator(const char *typestr) {
    Tok id;
    int arrsz = 0;

    if (!match("IDENTIFIER", &id)) {
        syn_error("Identifier expected");
        return;
    }

    array_opt(&arrsz);
    init_opt(typestr, id.line);
    add_symbol(id.lexeme, typestr, cur_scope, arrsz, id.line);
}

/* init_declarator_list: init_declarator { ',' init_declarator } */
static void init_declarator_list(const char *typestr) {
    init_declarator(typestr);
    while (match("COMMA", NULL)) {
        init_declarator(typestr);
    }
}

/* declaration: type_specifier init_declarator_list ';' */
static void declaration(const char *typestr) {
    init_declarator_list(typestr);
    if (!match("SEMICOLON", NULL)) {
        syn_error("Semicolon expected");
    }
}

/* global_decl_list: { type_specifier (not followed by MAIN) declaration } */
static void global_decl_list(void) {
    for (;;) {
        Tok t = LA();
        char ty[16];

        if (!is_type_token(t.token)) {
            return;
        }

        strcpy(ty, norm_type_token(t.token));
        consume();

        t = LA();
        if (strcmp(t.token, "MAIN") == 0) {
            pos--;
            return;
        }

        declaration(ty);
    }
}

/* stmt_list_opt: { statement } */
static void stmt_list_opt(void) {
    for (;;) {
        Tok t = LA();

        if (strcmp(t.token, "RBRACE") == 0 ||
            strcmp(t.token, "EOF")    == 0) {
            return;
        }

        statement();
    }
}

/* block: '{' stmt_list_opt '}' */
static void block(void) {
    if (!match("LBRACE", NULL)) {
        syn_error("{ expected");
        return;
    }

    stmt_list_opt();

    if (!match("RBRACE", NULL)) {
        syn_error("} missing");
    }
}

/* expr_stmt: expression ';' | ';' */
static void expr_stmt(void) {
    Tok t = LA();

    if (strcmp(t.token, "SEMICOLON") == 0) {
        consume();
        return;
    }

    {
        int etype = TYPE_ERROR;
        if (!expression_if_any(&etype)) {
            syn_error("Identifier or integer constant expected");
        }
    }

    if (!match("SEMICOLON", NULL)) {
        syn_error("Semicolon expected");
    }
}

/* if_stmt: IF '(' expression ')' block [ ELSE block ] */
static void if_stmt(void) {
    int cond_type = TYPE_ERROR;

    if (!match("IF", NULL)) {
        syn_error("IF expected");
        return;
    }

    if (!match("LPAREN", NULL)) {
        syn_error("Opening parenthesis missing");
    }

    if (!expression_if_any(&cond_type)) {
        syn_error("Identifier or integer constant expected");
    } else if (cond_type != TYPE_INT && cond_type != TYPE_ERROR) {
        Tok t = LA();
        semantic_error("Integer expected in conditional expression.", t.line);
    }

    if (!match("RPAREN", NULL)) {
        syn_error("Closing parenthesis missing");
    }

    block();

    if (match("ELSE", NULL)) {
        block();
    }
}

/* while_stmt: WHILE '(' expression ')' block */
static void while_stmt(void) {
    int cond_type = TYPE_ERROR;

    if (!match("WHILE", NULL)) {
        syn_error("WHILE expected");
        return;
    }

    if (!match("LPAREN", NULL)) {
        syn_error("Opening parenthesis missing");
    }

    if (!expression_if_any(&cond_type)) {
        syn_error("Identifier or integer constant expected");
    } else if (cond_type != TYPE_INT && cond_type != TYPE_ERROR) {
        Tok t = LA();
        semantic_error("Integer expected in conditional expression.", t.line);
    }

    if (!match("RPAREN", NULL)) {
        syn_error("Closing parenthesis missing");
    }

    block();
}

/* for_stmt: FOR '(' expression ';' expression ';' expression ')' statement */
static void for_stmt(void) {
    int e1_type   = TYPE_ERROR;
    int cond_type = TYPE_ERROR;
    int e3_type   = TYPE_ERROR;

    if (!match("FOR", NULL)) {
        syn_error("FOR expected");
        return;
    }

    if (!match("LPAREN", NULL)) {
        syn_error("Opening parenthesis missing");
    }

    if (!expression_if_any(&e1_type)) {
        syn_error("Identifier or integer constant expected");
    }

    if (!match("SEMICOLON", NULL)) {
        syn_error("Semicolon expected");
    }

    if (!expression_if_any(&cond_type)) {
        syn_error("Identifier or integer constant expected");
    } else if (cond_type != TYPE_INT && cond_type != TYPE_ERROR) {
        Tok t = LA();
        semantic_error("Integer expected in conditional expression.", t.line);
    }

    if (!match("SEMICOLON", NULL)) {
        syn_error("Semicolon expected");
    }

    if (!expression_if_any(&e3_type)) {
        syn_error("Identifier or integer constant expected");
    }

    if (!match("RPAREN", NULL)) {
        syn_error("Closing parenthesis missing");
    }

    statement();
}

/* statement dispatcher */
static void statement(void) {
    Tok t = LA();

    if (is_type_token(t.token)) {
        char ty[16];
        type_specifier(ty);
        declaration(ty);
    } else if (strcmp(t.token, "IF") == 0) {
        if_stmt();
    } else if (strcmp(t.token, "WHILE") == 0) {
        while_stmt();
    } else if (strcmp(t.token, "FOR") == 0) {
        for_stmt();
    } else if (strcmp(t.token, "LBRACE") == 0) {
        block();
    } else {
        expr_stmt();
    }
}

/* function_def: type_specifier MAIN '(' [ VOID | param_list ] ')' block */
static void function_def(const char *ret_type) {
    Tok id;

    if (!match("MAIN", &id)) {
        syn_error("MAIN expected");
        return;
    }

    add_symbol("main", "Function", "Global", 0, id.line);
    strcpy(cur_scope, "Main");

    if (!match("LPAREN", NULL)) {
        syn_error("Opening parenthesis missing");
    }

    {
        Tok t = LA();

        if (strcmp(t.token, "VOID") == 0) {
            consume();
        } else if (is_type_token(t.token)) {
            for (;;) {
                char pty[16];
                Tok  pid;

                if (!type_specifier(pty)) {
                    syn_error("Any keyword expected");
                    break;
                }

                if (!match("IDENTIFIER", &pid)) {
                    syn_error("Identifier expected");
                    break;
                }

                add_symbol(pid.lexeme, pty, "Main", 0, pid.line);

                if (!match("COMMA", NULL)) {
                    break;
                }
            }
        }
    }

    if (!match("RPAREN", NULL)) {
        syn_error("Closing parenthesis missing");
    }

    block();
    strcpy(cur_scope, "Global");
}

/* program: global_decl_list function_def */
static void program(void) {
    char rtype[16];

    global_decl_list();

    if (!type_specifier(rtype)) {
        syn_error("Any keyword expected");
        return;
    }

    function_def(rtype);
}

int main(void) {
    if (!load_tokens("tokens.txt")) {
        return 1;
    }

    program();
    write_symbol_table("symbol_table_semantic.txt");

    if (error_count == 0) {
        printf("Semantic analysis finished with no errors.\n");
    } else {
        printf("Semantic analysis finished with %d error(s).\n", error_count);
    }

    return 0;
}