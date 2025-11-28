#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct {
    char token[32];
    char lexeme[256];
    int line;
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

static void add_symbol(const char *name, const char *type, const char *scope, int arrsz) {
    if (nsym >= MAXSYM) return;
    snprintf(symtab[nsym].lexeme, sizeof(symtab[nsym].lexeme), "%s", name);
    snprintf(symtab[nsym].type,   sizeof(symtab[nsym].type),   "%s", type);
    snprintf(symtab[nsym].scope,  sizeof(symtab[nsym].scope),  "%s", scope);
    symtab[nsym].array_size = arrsz;
    nsym++;
}

static Tok LA(void) { return (pos < ntok) ? toks[pos] : (Tok){"EOF", "", 999999}; }
static Tok consume(void) { return (pos < ntok) ? toks[pos++] : (Tok){"EOF", "", 999999}; }
static int match(const char *tk, Tok *out) {
    Tok a = LA();
    if (strcmp(a.token, tk) == 0) { if (out) *out=a; consume(); return 1; }
    return 0;
}

static void skip_line_tokens(int line) {
    while (pos < ntok && toks[pos].line == line) pos++;
}

static void syn_error(const char *msg) {
    Tok t = LA();
    fprintf(stderr, "Line %d: %s\n", t.line, msg);
    error_count++;
    skip_line_tokens(t.line);
}

/* Forward declarations: */
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
static int  expression_if_any(void);
static int  is_operator(const char *tk);

/* Helpers */
static const char* norm_type_token(const char *tk) {
    if (strcmp(tk,"VOID")==0) return "Void";
    if (strcmp(tk,"CHAR")==0) return "Char";
    if (strcmp(tk,"INT")==0)  return "Int";
    return "?";
}

static int is_type_token(const char *tk) {
    return strcmp(tk,"VOID")==0 || strcmp(tk,"CHAR")==0 || strcmp(tk,"INT")==0;
}

/* program: global_decl_list function_def */
static void program(void) {
    global_decl_list();

    char ftype[16];
    if (!type_specifier(ftype)) { syn_error("Any keyword expected"); return; }
    function_def(ftype);
}

/* Parse repeated global declarations until a type followed by MAIN is seen */
static void global_decl_list(void) {
    for (;;) {
        Tok t = LA();
        if (!is_type_token(t.token)) return;

        /* Lookahead to see if this starts the function_def: type MAIN */
        Tok save = t;
        consume();
        Tok t2 = LA();
        pos--;
        if (strcmp(t2.token, "MAIN") == 0) return;

        char ty[16];
        if (!type_specifier(ty)) { syn_error("Any keyword expected"); return; }
        declaration(ty);
    }
}

/* type_specifier: VOID | CHAR | INT  -> writes normalized name */
static int type_specifier(char *out) {
    Tok t = LA();
    if (strcmp(t.token,"VOID")==0 || strcmp(t.token,"CHAR")==0 || strcmp(t.token,"INT")==0) {
        const char *n = norm_type_token(t.token);
        strcpy(out, n);
        consume();
        return 1;
    }
    return 0;
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
    int arrsz = -1;
    array_opt(&arrsz);
    init_opt();
    add_symbol(id.lexeme, typestr, cur_scope, arrsz);
}

/* array_opt: empty | '[' INT_CONST? ']' */
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
    if (strcmp(t.token,"INT_CONST")==0 || strcmp(t.token,"CHAR_CONST")==0) { consume(); return 1; }
    syn_error("Identifier or integer constant expected");
    return 1;
}

/* function_def: type_specifier MAIN '(' type_specifier ')' '{' stmt_list_opt '}' */
static void function_def(const char *ret_type) {
    if (!match("MAIN", NULL)) { syn_error("MAIN expected"); return; }
    add_symbol("main", "Function", "Global", -1);

    if (!match("LPAREN", NULL)) syn_error("Opening parenthesis missing");
    char pty[16];
    if (!type_specifier(pty)) syn_error("Any keyword expected");
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

/* statement: block | declaration | expr_stmt | if_stmt | while_stmt | for_stmt */
static void statement(void) {
    Tok t = LA();
    if (strcmp(t.token,"LBRACE")==0) { block(); return; }
    if (strcmp(t.token,"IF")==0)     { if_stmt(); return; }
    if (strcmp(t.token,"WHILE")==0)  { while_stmt(); return; }
    if (strcmp(t.token,"FOR")==0)    { for_stmt(); return; }
    if (is_type_token(t.token)) {
        char ty[16]; if (!type_specifier(ty)) { syn_error("Any keyword expected"); return; }
        declaration(ty); return;
    }
    expr_stmt();
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
    if (!expression_if_any()) syn_error("Identifier or integer constant expected");
    if (!match("SEMICOLON", NULL)) syn_error("Semicolon expected");
}

/* if_stmt: IF '(' expression ')' block [ ELSE block ] */
static void if_stmt(void) {
    if (!match("IF", NULL)) { syn_error("IF expected"); return; }
    if (!match("LPAREN", NULL)) syn_error("Opening parenthesis missing");
    if (!expression_if_any()) syn_error("Identifier or integer constant expected");
    if (!match("RPAREN", NULL)) syn_error("Closing parenthesis missing");
    block();
    if (match("ELSE", NULL)) block();
}

/* while_stmt: WHILE '(' expression ')' block */
static void while_stmt(void) {
    if (!match("WHILE", NULL)) { syn_error("WHILE expected"); return; }
    if (!match("LPAREN", NULL)) syn_error("Opening parenthesis missing");
    if (!expression_if_any()) syn_error("Identifier or integer constant expected");
    if (!match("RPAREN", NULL)) syn_error("Closing parenthesis missing");
    block();
}

/* for_stmt: FOR '(' expression ';' expression ';' expression ')' statement */
static void for_stmt(void) {
    if (!match("FOR", NULL)) { syn_error("FOR expected"); return; }
    if (!match("LPAREN", NULL)) syn_error("Opening parenthesis missing");
    if (!expression_if_any()) syn_error("Identifier or integer constant expected");
    if (!match("SEMICOLON", NULL)) syn_error("Semicolon expected");
    if (!expression_if_any()) syn_error("Identifier or integer constant expected");
    if (!match("SEMICOLON", NULL)) syn_error("Semicolon expected");
    if (!expression_if_any()) syn_error("Identifier or integer constant expected");
    if (!match("RPAREN", NULL)) syn_error("Closing parenthesis missing");
    statement();
}

/* expression: (IDENTIFIER|INT_CONST|CHAR_CONST) { op (IDENTIFIER|INT_CONST|CHAR_CONST) } */
static int expression_if_any(void) {
    Tok a = LA();
    if (strcmp(a.token,"IDENTIFIER")==0 ||
        strcmp(a.token,"INT_CONST")==0 ||
        strcmp(a.token,"CHAR_CONST")==0) {
        consume();
    } else {
        return 0;
    }
    for (;;) {
        Tok op = LA();
        if (!is_operator(op.token)) break;
        consume();
        Tok b = LA();
        if (strcmp(b.token,"IDENTIFIER")==0 ||
            strcmp(b.token,"INT_CONST")==0 ||
            strcmp(b.token,"CHAR_CONST")==0) {
            consume();
        } else {
            syn_error("Identifier or integer constant expected");
            break;
        }
    }
    return 1;
}

static int is_operator(const char *tk) {
    return strcmp(tk,"PLUS")==0 || strcmp(tk,"MINUS")==0 || strcmp(tk,"STAR")==0 ||
           strcmp(tk,"SLASH")==0 || strcmp(tk,"GT")==0   || strcmp(tk,"LT")==0   ||
           strcmp(tk,"ASSIGN")==0 || strcmp(tk,"EQ")==0;
}

/* Token file reader: tolerates presence of a header line "Token  Lexeme  Line No" */
static int read_tokens(const char *fname) {
    FILE *f = fopen(fname, "r");
    if (!f) return 0;
    char line[2048];
    int first = 1;
    while (fgets(line, sizeof(line), f)) {
        if (first) {
            first = 0;
            char h1[32], h2[32], h3[32];
            if (sscanf(line, " %31s %31s %31s", h1, h2, h3) == 3) {
                if ((strcmp(h1,"Token")==0 || strcmp(h1,"TOKEN")==0) &&
                    (strcmp(h2,"Lexeme")==0 || strcmp(h2,"LEXEME")==0) &&
                    (strcmp(h3,"Line")==0 || strstr(h3,"Line")==h3)) {
                    continue; /* skip header */
                }
            }
        }
        char tkn[64], lex[512];
        int ln = 0;

        char *p = line;
        while (*p && isspace((unsigned char)*p)) p++;

        int i=0; while (*p && !isspace((unsigned char)*p) && i<63) tkn[i++]=*p++;
        tkn[i]=0;
        while (*p && isspace((unsigned char)*p)) p++;

        i=0; while (*p && *p!='\t' && *p!='\n' && i<511) lex[i++]=*p++;
        lex[i]=0;
        while (*p && isspace((unsigned char)*p)) p++;

        if (*p) ln = atoi(p);

        if (tkn[0]==0) continue;

        if (ntok < MAXTOK) {
            strncpy(toks[ntok].token, tkn, sizeof(toks[ntok].token)-1);
            toks[ntok].token[sizeof(toks[ntok].token)-1]=0;
            strncpy(toks[ntok].lexeme, lex, sizeof(toks[ntok].lexeme)-1);
            toks[ntok].lexeme[sizeof(toks[ntok].lexeme)-1]=0;
            toks[ntok].line = ln ? ln : 0;
            ntok++;
        }
    }
    fclose(f);
    return 1;
}

static void print_symbol_table(void) {
    FILE *out = fopen("symbol_table.txt", "w");
    if (!out) {
        fprintf(stderr, "Failed to create symbol_table.txt\n");
        return;
    }

    fprintf(out, "Lexeme\tType\tScope\tArray size\n");
    for (int i = 0; i < nsym; i++) {
        fprintf(out, "%s\t%s\t%s\t", symtab[i].lexeme, symtab[i].type, symtab[i].scope);
        if (symtab[i].array_size >= 0)
            fprintf(out, "%d\n", symtab[i].array_size);
        else
            fprintf(out, "\n");
    }

    fclose(out);
    printf("Symbol table written to symbol_table.txt\n");
}

int main(void) {
    if (!read_tokens("tokens.txt")) {
        fprintf(stderr, "Failed to open tokens.txt\n");
        return 1;
    }
    program();
    print_symbol_table();
    return 0;
}