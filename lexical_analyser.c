#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct {
    const char *name;
} TokName;

static int line_no = 1;
static FILE *in;
static FILE *out;

/* Utility: write a token row */
static void emit(const char *tok, const char *lex, int line) {
    fprintf(out, "%s\t%s\t%d\n", tok, lex ? lex : "", line);
}

/* Read next character with line tracking */
static int getc_track(void) {
    int c = fgetc(in);
    if (c == '\n') line_no++;
    return c;
}

/* Push back a character with line tracking fix */
static void ungetc_track(int c) {
    if (c == EOF) return;
    if (c == '\n') line_no--;
    ungetc(c, in);
}

static int is_ident_start(int c) { return isalpha(c) || c == '_'; }
static int is_ident_part (int c) { return isalnum(c) || c == '_'; }

/* Check for Keywords */
static const char* keyword_or_ident(const char *lex) {
    char temp[64];
    for (int i=0; lex[i] && i<63; i++) temp[i]=toupper((unsigned char)lex[i]);
    temp[strlen(lex)]='\0';
    if (strcmp(temp,"VOID")==0)  return "VOID";
    if (strcmp(temp,"CHAR")==0)  return "CHAR";
    if (strcmp(temp,"INT")==0)   return "INT";
    if (strcmp(temp,"IF")==0)    return "IF";
    if (strcmp(temp,"ELSE")==0)  return "ELSE";
    if (strcmp(temp,"WHILE")==0) return "WHILE";
    if (strcmp(temp,"FOR")==0)   return "FOR";
    if (strcmp(temp,"MAIN")==0)  return "MAIN";
    return "IDENTIFIER";
}

static void skip_ws_and_comments(void) {
    for (;;) {
        int c = getc_track();
        if (c == EOF) return;
        if (c==' '||c=='\t'||c=='\r'||c=='\v'||c=='\f'||c=='\n') continue;

        if (c == '/') {
            int d = getc_track();
            if (d == '*') {
                int prev = 0, cur;
                int start_line = line_no;
                int closed = 0;
                while ((cur = getc_track()) != EOF) {
                    if (prev == '*' && cur == '/') { closed = 1; break; }
                    prev = cur;
                }
                if (!closed) {
                    fprintf(stderr, "Line %d: Un-terminated comments\n", start_line);
                    return;
                }
                continue;
            } else {
                ungetc_track(d);
                ungetc_track(c);
                return;
            }
        }

        ungetc_track(c);
        return;
    }
}

static void scan_string(void) {
    char buf[1024]; int i=0;
    int start_line = line_no;
    for (;;) {
        int c = getc_track();
        if (c == EOF || c == '\n') {
            fprintf(stderr, "Line %d: String constants exceed line\n", start_line);
            return;
        }
        if (c == '"') {
            buf[i] = 0;
            emit("STRING_CONST", buf, start_line);
            return;
        }
        if (c == '\\') {
            int e = getc_track();
            if (e == EOF || e == '\n') {
                fprintf(stderr, "Line %d: String constants exceed line\n", start_line);
                return;
            }
            if (i < (int)sizeof(buf)-2) { buf[i++]='\\'; buf[i++]=(char)e; }
        } else {
            if (i < (int)sizeof(buf)-1) buf[i++] = (char)c;
        }
    }
}

static void scan_char(void) {
    char buf[8]; int i=0;
    int start_line = line_no;
    int c = getc_track();
    if (c == '\\') {
        int e = getc_track();
        if (e == EOF || e == '\n') {
            fprintf(stderr, "Line %d: Char constant too long\n", start_line);
            return;
        }
        buf[i++]='\\'; buf[i++]=(char)e;
        c = getc_track();
        if (c != '\'') {
            fprintf(stderr, "Line %d: Char constant too long\n", start_line);
            while (c != EOF && c != '\n' && c != '\'') c = getc_track();
            return;
        }
        buf[i]=0;
        emit("CHAR_CONST", buf, start_line);
    } else if (c == '\'' || c == '\n' || c == EOF) {
        fprintf(stderr, "Line %d: Char constant too long\n", start_line);
        return;
    } else {
        int end = getc_track();
        if (end != '\'') {
            fprintf(stderr, "Line %d: Char constant too long\n", start_line);
            while (end != EOF && end != '\n' && end != '\'') end = getc_track();
            return;
        }
        buf[0]=(char)c; buf[1]=0;
        emit("CHAR_CONST", buf, start_line);
    }
}

static void skip_line_after_error(void) {
    int c;
    while ((c = getc_track()) != EOF) {
        if (c == '\n') break;
    }
}

int main(int argc, char **argv) {
    const char *infile = NULL;
    if (argc > 1) infile = argv[1];
    in  = infile ? fopen(infile, "r") : stdin;
    if (!in) { fprintf(stderr, "Failed to open input file.\n"); return 1; }
    out = fopen("tokens.txt", "w");
    if (!out) { fprintf(stderr, "Failed to open tokens.txt for writing.\n"); return 1; }

    fprintf(out, "Token\tLexeme\tLine No\n");

    for (;;) {
        skip_ws_and_comments();
        int c = getc_track();
        if (c == EOF) break;

        if (is_ident_start(c)) {
            char buf[256]; int i=0;
            buf[i++]=(char)c;
            for (;;) {
                int d = getc_track();
                if (!is_ident_part(d)) { ungetc_track(d); break; }
                if (i < (int)sizeof(buf)-1) buf[i++]=(char)d;
            }
            buf[i]=0;
            const char *tk = keyword_or_ident(buf);
            emit(tk, buf, line_no);
            continue;
        }

        if (isdigit(c)) {
            char buf[256]; int i=0;
            buf[i++]=(char)c;
            for (;;) {
                int d = getc_track();
                if (!isdigit(d)) { ungetc_track(d); break; }
                if (i < (int)sizeof(buf)-1) buf[i++]=(char)d;
            }
            buf[i]=0;
            emit("INT_CONST", buf, line_no);
            continue;
        }

        if (c == '"') { scan_string(); continue; }
        if (c == '\''){ scan_char(); continue; }

        if (c == '=') {
            int d = getc_track();
            if (d == '=') emit("EQ", "==", line_no);
            else { ungetc_track(d); emit("ASSIGN", "=", line_no); }
            continue;
        }
        if (c == '+') { emit("PLUS", "+", line_no); continue; }
        if (c == '-') { emit("MINUS","-", line_no); continue; }
        if (c == '*') { emit("STAR", "*", line_no); continue; }
        if (c == '/') { emit("SLASH","/", line_no); continue; }
        if (c == '>') { emit("GT", ">", line_no); continue; }
        if (c == '<') { emit("LT", "<", line_no); continue; }

        if (c == '(') { emit("LPAREN","(", line_no); continue; }
        if (c == ')') { emit("RPAREN",")", line_no); continue; }
        if (c == '{') { emit("LBRACE","{", line_no); continue; }
        if (c == '}') { emit("RBRACE","}", line_no); continue; }
        if (c == '[') { emit("LBRACKET","[", line_no); continue; }
        if (c == ']') { emit("RBRACKET","]", line_no); continue; }
        if (c == ';') { emit("SEMICOLON",";", line_no); continue; }
        if (c == ',') { emit("COMMA",",", line_no); continue; }

        fprintf(stderr, "Line %d: Undefined symbol\n", line_no);
        skip_line_after_error();
    }

    fclose(out);
    if (in && in != stdin) fclose(in);
    return 0;
}