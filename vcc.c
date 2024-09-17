#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* UTIL */

char *infile = 0;
int lineno;
int startline;

void errh(int n) {
    if(infile) printf("%s:%d: ", infile, n);
}

void errr(unsigned short n) {
    errh(startline+n);
}

FILE *openFile(char *filename, const char *m) {
    FILE *fp = fopen(filename, m);
    if(!fp) { errh(lineno); printf("failed to open %s\n",filename); exit(1); }
    return fp;
}

/* PARSER */

#define MAXSYMS 3000
#define SYMBUFSZ 4096
#define MAXTOKENS 3000
#define BUFSZ 500

char *defsyms[] = {
    "+","-","&","|","^","~","!","*","/","%","<",">","=","<<",">>",
    "+=","-=","&=","|=","^=","?","!=","*=","/=","%=",
    "<=",">=","==","<<=",">>=",
    "||","&&","++","--","(",")","{","}","[","]",":",";", ",", ".","->",
    "void","char","short","int","long","float","auto","unsigned",
    "struct","union","enum","typedef",
    "if","else","for","while","do","switch","case","default",
    "break","continue","return",
    "*X","&X","++X","--X","-X","+X",
    "<macro>","<cast>","<call>","<defun>","<defvar>","<ex>",
};

enum {
    P=0,M,AND,OR,XOR,INV,NOT,MUL,DIV,REM,LT,GT,EQ,SHL,SHR,
    PE,ME,ANDE,ORE,XORE,TERN,NOTE,MULE,DIVE,REME,
    LTE,GTE,EQE,SHLE,SHRE,
    OOR,AAND,PP,MM,LP,RP,LC,RC,LB,RB,COL,SEMI,COM,DOT,AR,
    VOID,CHAR,SHORT,INT,LONG,FLOAT,AUTO,UNSIGNED,
    STRUCT,UNION,ENUM,TYPEDEF,
    IF,ELSE,FOR,WHILE,DO,SWITCH,CASE,DEFAULT,
    BREAK,CONTINUE,RETURN,
    DER,REF,PPX,MMX,MX,PX,
    MACRO,CAST,CALL,DEFUN,DEFVAR,EX,
    NDEF,
};

short symbols[MAXSYMS]; // maybe clean up symbols at each EOF?
int nsymbols=0;

char symbuf[SYMBUFSZ];
int nsymbuf=0;

char *symbol(int s) {
    if(s == EOF) return "<EOF>";
    if((s-=NDEF) < 0) return defsyms[s+NDEF];
    if(s >= nsymbols) return "?invalid";
    return &symbuf[symbols[s]];
}

int toSymbol(char *s) {
    int i;
    for(i = 0; i < NDEF; i++) if(!strcmp(defsyms[i], s)) return i;
    for(i = 0; i < nsymbols; i++) if(!strcmp(&symbuf[symbols[i]], s)) return i+NDEF;
    strcpy(&symbuf[nsymbuf], s);
    symbols[nsymbols++] = nsymbuf;
    nsymbuf += strlen(s)+1;
    return nsymbols-1+NDEF;
}

int digit(char c) {
    if(c >= '0' && c <= '9') return (c-'0');
    if(c >= 'A' && c <= 'F') return (c-'A'+10);
    if(c >= 'a' && c <= 'f') return (c-'a'+10);
    return 0;
}

char escapeChar(char **pbuf) {
    char c, d;
    char *buf = *pbuf;
    switch(c = *(buf++)) {
    case 'n': c = 10; break;
    case 't': c = '\t'; break;
    case 'r': c = '\r'; break;
    case 'b': c = '\b'; break;
    case 'v': c = '\v'; break;
    case 'f': c = '\f'; break;
    case 'x':
        if((c = digit(*(buf++))) == -1) {
            buf--; c = 'x';
        } else if((d = digit(*(buf++))) == -1) {
            buf -= 2; c = 'x';
        } else c = c<<4|d;
        break;
    default:
        if(c >= '0' && c <= '7') {
            d = 0;
            do {
                d = d<<3 | (c-'0');
                c = *(buf++);
            } while(c >= '0' && c <= '7');
        }
        break;
    }
    *pbuf = buf;
    return c;
}

void parseString(FILE *fp, char *buf, char q) {
    char c;
    *(buf++) = q;
    for(;;) {
        c = fgetc(fp);
        if(c == q) { *buf = 0; return; }
        if(c == '\n' || c == EOF) {
            errh(lineno); printf("unterminated quote\n"); exit(1);
        }
        if(c == '\\') {
            *(buf++) = c;
            c = fgetc(fp);
            if(c == '\n' || c == EOF) {
                errh(lineno); printf("unterminated quote\n"); exit(1);
            }
        }
        *(buf++) = c;
    }
}

int extractChar(char *buf) {
    int d, c = 0;
    while(*buf) {
        if((d = *(buf++)) == '\\') d = escapeChar(&buf);
        c = c<<8|d;
    }
    return c;
}

void parseNext(FILE *fp, char *buf) {
    static char ahc = 0;
    char c;
    int i = 0;
    if(ahc) { c = ahc; ahc = 0; goto got; }
    for(;;) {
        c = fgetc(fp);
got:
        if(c == EOF) { buf[i] = 0; return; }
        if(c <= 32) {
            if(i) { ahc = c; buf[i] = 0; return; }
            if(c == 10) lineno++;
        } else if(strchr("+-&|^~!*/%<>=", c)) {
            if(i) { ahc = c; buf[i] = 0; return; }
            buf[0] = c;
            c = fgetc(fp);
            if(buf[0] == '/' && c == '*') {
                for(;;) {
                    c = fgetc(fp);
                    if(c == '*') { if((c = fgetc(fp)) == '/') break; }
                    if(c == '\n') lineno++;
                }
                continue;
            }
            if(c == '=') { buf[1] = c; buf[2] = 0; return; }
            if(buf[0] == c && strchr("<>+-&|/", c)) {
                if(c == '/') {
                    while(fgetc(fp) != '\n'); lineno++; continue;
                }
                buf[1] = c;
                if(c == '<' || c == '>') {
                    if((c = fgetc(fp)) == '=') { buf[2] = c; buf[3] = 0; }
                    else { ahc = c; buf[2] = 0; }
                } else buf[2] = 0;
                return;
            }
            if(buf[0] == '-' && c == '>') { buf[1] = '>'; buf[2] = 0; return; }
            ahc = c; buf[1] = 0; return;
        } else if(strchr("(){}[]:;,.?", c)) {
            if(i) { ahc = c; buf[i] = 0; }
            else { buf[0] = c; buf[1] = 0; }
            return;
        } else if(c == '"' || c == '\'') {
            if(i) { ahc = c; buf[i] = 0; }
            else parseString(fp, buf, c);
            return;
        } else buf[i++] = c;
    }
}

int nextSymbol(FILE *fp, char *buf) {
    parseNext(fp, buf);
    if(!buf[0]) return EOF;
    return toSymbol(buf);
}

int parseTokens(FILE *fp, char *buf, short *tokens, unsigned char *offs) {
    static short aht = EOF;
    static int ahl;
    char f = 0;
    int d = 0, p;
    int ntokens = 0;
    int t;
    if(aht != EOF) { t = aht; lineno = ahl; aht = EOF; }
    else { t = nextSymbol(fp, buf); if(t == EOF) return 0; }
    startline = p = lineno;
    if(*symbol(t) == '#') {
        d = lineno;
        do {
            offs[ntokens] = 0;
            tokens[ntokens++] = t;
            t = nextSymbol(fp, buf);
        } while(d == lineno && t != EOF);
        if(t != EOF) { aht = t; ahl = lineno; lineno = d; }
        tokens[ntokens++] = EOF;
        return ntokens;
    }
    goto got;
    for(;;) {
        t = nextSymbol(fp, buf);
got:
        offs[ntokens] = lineno-p;
        p = lineno;
        tokens[ntokens++] = t;
        if(t == RP && !(d|f)) {
            tokens[ntokens++] = t = nextSymbol(fp, buf);
            if(t == LC) { f = 1; d++; continue; }
        }
        if(t == EOF) break;
        else if(t == LC) d++;
        else if(t == RC) { if(!(--d) && f) break; }
        else if(t == SEMI && !d) break;
    }
    tokens[ntokens++] = EOF;
    return ntokens;
}

/* TREE */

#define MAXTYPES 200
#define MAXDIM 100
#define MAXCELLS 2000

enum { LLIST=0, LINT, LSYM, LSTR, LTYPE, };

typedef struct {
    unsigned short t;
    union {
        struct { short car; short cdr; };
        struct { short sym; unsigned short lo; };
        int n;
    };
} cell;

cell _cells[MAXCELLS];
cell *cells = _cells-1;
int ncells;

typedef struct tocon {
    int i;
    int line;
    int ntokens;
    short *tokens;
    unsigned char *offs;
} tocon;

enum {
  TVOID=-9, TCHAR, TSHORT, TINT, TLONG, TUCHAR, TUSHORT, TUINT, TULONG,

  TPTR=0, TARR, TFUN, TSTRUCT, TUNION,
  TNOT=-10,
};

typedef struct {
    char t;
    short i;
    union {
        int p;
        struct {
            unsigned char n; short *e;
        };
    };
} datatype;
datatype types[MAXTYPES];
int ntypes = 0;
short els[MAXDIM];
int nels = 0;

int opIn(short *as, short s) {
    int i;
    for(i = 0; as[i] != -1; i++) if(as[i] == s) return 1;
    return 0;
}

int inccon(tocon *con) {
    return (con->line += con->offs[con->i++]);
}

void printCell(short c);

short cons(short a, short b) {
    short l = ncells++;
    cells[l].t = LLIST;
    cells[l].car = a;
    cells[l].cdr = b;
    return l;
}

void append(short a, short b) {
    b = cons(b, 0);
    while(cells[a].cdr) {
        a = cells[a].cdr;
    }
    cells[a].cdr = b;
}

short car(short c) { return cells[c].car; }
short cdr(short c) { return cells[c].cdr; }
short cellt(short c) { return cells[c].t; }
short cellsym(short c) { return cells[c].sym; }
short cello(short c) { return cells[c].lo; }

short symCell(char t, short sym, short lo) {
    cell *l = &cells[ncells];
    l->t = t; l->sym = sym; l->lo = lo;
    return ncells++;
}

short nextSymCell(tocon *con) {
    short t = con->tokens[con->i];
    return symCell(LSYM, t, inccon(con));
}

short nextSym(tocon *con) {
    short t = con->tokens[con->i];
    inccon(con);
    return t;
}

 /* So, this was taken from some scheme I wrote sometime
 - it was wierd then, too. */
short ex(tocon *con, short **ops, short (*fall)(tocon*)) {
    short c, a = (*ops) ? ex(con, ops+1, fall) : fall(con);
    int i;
    for(i = 0; ops[i]; i++)
        if(opIn(ops[i], con->tokens[con->i])) {
            i = nextSymCell(con);
            c = ex(con, ops, fall);
            return cons(i, cons(a, cons(c, 0)));
        }
    return a;
}

int hex(char *s, int *n) {
    do {
        *n <<= 4;
        if(*s >= '0' && *s <= '9') *n |= *s - '0';
        else if(*s >= 'a' && *s <= 'f') *n |= *s - 'a' + 10;
        else if(*s >= 'A' && *s <= 'F') *n |= *s - 'A' + 10;
        else return 0;
    } while(*(++s));
    return 1;
}

int oct(char *s, int *n) {
    if(*s == 'x') return hex(s+1, n);
    do {
        *n <<= 3;
        if(*s >= '0' && *s <= '7') *n |= *s - '0';
        else return 0;
    } while(*(++s));
    return 1;
}

int number(char *s, int *n) {
    *n = 0;
    if(*s == '0') return oct(s, n);
    do {
        *n *= 10;
        if(*s >= '0' && *s <= '9') *n += *s - '0';
        else return 0;
    } while(*(++s));
    return 1;
}

short uncomma(short c) {
    short a;
    if(cells[c].t == LLIST && cells[a=cells[c].car].t == LSYM && cells[a].sym == COM) {
        c = cells[c].cdr;
        return cons(cells[c].car, uncomma(cells[c].cdr));
    }
    if(cells[c].t != LLIST) return cons(c, 0);
    return c;
}

short ex0(tocon *con);
short getType(tocon *con);

short expre(tocon *con) {
    int t;
    cell *l;
    short a, c = ncells++;
    l = &cells[c];
    l->t = LSYM;
    l->lo = inccon(con);
    switch(t = con->tokens[con->i-1]) {
    case PP: l->sym = PPX; break;
    case MM: l->sym = MMX; break;
    case MUL: l->sym = DER; break;
    case AND: l->sym = REF; break;
    case P: l->sym = PX; break;
    case M: l->sym = MX; break;
    case NOT: case INV: l->sym = t; break;
    default:
        if(t == LP) {
            ncells--; l = &cells[c = ex0(con)];
            if(con->tokens[con->i] != RP) {
                errr(con->line); printf("expected )\n"); exit(1);
            }
            inccon(con);
            if(l->t == LLIST && l->car && cells[l->car].t == LTYPE) {
                c = cons(symCell(LSYM,CAST,l->lo), cons(c, cons(ex0(con), 0)));
            }
        } else {
            l->sym = t;
            if(number(symbol(t), &t)) {
                l->t = LINT;
                l->n = t;
            } else if(symbol(l->t)[0] == '"') {
                l->t = LSTR;
            } else {
                con->line -= con->offs[--con->i];
                if((t = getType(con)) != TNOT) {
                    l->t = LTYPE;
                    l->sym = t;
                    c = cons(c, cons(ex0(con), 0));
                } else inccon(con);
            }
        }
        for(;;) {
            switch(t = con->tokens[con->i]) {
            case LP:
                a = symCell(LSYM,CALL,inccon(con));
                c = cons(a, cons(c, uncomma(ex0(con))));
                if(nextSym(con) != RP) {
                    errr(cells[t].lo); printf("expected )\n"); exit(1);
                }
                break;
            case LB:
                a = symCell(LSYM,CALL,inccon(con));
                c = cons(a, cons(c, cons(ex0(con), 0)));
                if(nextSym(con) != RB) { errr(cells[c].lo);
                    printf("expected ]\n"); exit(1); }
                break;
            case PP: case MM:
                c = cons(nextSymCell(con), cons(c, 0));
                break;
            default:
                return c;
            }
        }
    }
    return cons(c, cons(expre(con), 0));
}

short tern(tocon *con) {
    static short oor[] = { OOR,-1, };
    static short aand[] = { AAND,-1, };
    static short or[] = { OR,-1, };
    static short xor[] = { XOR,-1, };
    static short and[] = { AND,-1, };
    static short eq[] = { EQ,NOTE,-1, };
    static short lt[] = { LT,LTE,GT,GTE,-1, };
    static short sh[] = { SHR,SHL,-1, };
    static short add[] = { P,M,-1, };
    static short div[] = { DIV,MUL,REM,-1, };
    static short *ops[] = { oor,aand,or,xor,and,eq,lt,sh,add,div,0, };
    short c, b, a = ex(con, ops, expre);
    cell *l;
    if(con->tokens[con->i] == TERN) {
        l = &cells[c = ncells++];
        l->t = LSYM;
        l->sym = TERN;
        l->lo = inccon(con);
        b = tern(con);
        if(con->tokens[con->i] != COL) {
            errr(con->line); printf("expected : after ?\n"); exit(1);
        }
        inccon(con);
        return cons(c, cons(a, cons(b, cons(tern(con), 0))));
    }
    return a;
}

short readMacro(tocon *con) {
    short c, lo;
    c = symCell(LSYM, MACRO, lo=inccon(con));
    c = cons(c, cons(symCell(LSYM, con->tokens[con->i-1], lo), 0));
    while(!con->offs[con->i])
        append(c, symCell(LSYM, con->tokens[con->i++], lo));
    return c;
}

int countEl(tocon *con) {
    int i, t, d=0, n=0;
    for(i = con->i+1;; i++) {
        if((t = con->tokens[i]) == LP || t == LC) d++;
        else if(t == RP || t == RC) { if(!(d--)) return n+(i!=con->i+1); }
        else if(!d && t == COM) n++;
    }
}

short getPrim(tocon *con) {
    char u = 0;
    int i = con->i;
    int t = con->tokens[con->i++];
    if(t == VOID) return TVOID;
    if(t == UNSIGNED) { u = 4; t = con->tokens[con->i++]; }
    if(t == LONG) {
        if(con->tokens[con->i++] == LONG) con->i++;
        return TLONG+u;
    }
    if(t >= CHAR && t <= INT) return t-INT-6+u;
    if(u) return TUINT;
    con->i = i;
    return 0;
}

short ptrType(int t, int p) {
    int i;
    datatype *d;
    for(i = 0; i < ntypes; i++)
        if((d = &types[i])->t == TPTR && d->i == t && d->p == p)
            return i;
    d = &types[ntypes];
    d->t = TPTR; d->i = t; d->p = p;
    return ntypes++;
}

short getType(tocon *con) {
    cell *l;
    int y, p, i=con->i;
    if(!(y = getPrim(con))) {
        return TNOT;
    }
    for(; i < con->i; i++) con->line += con->offs[i];
    con->i = i;
    p = 0;
    while(con->tokens[con->i] == MUL) {
        inccon(con); p++;
    }
    if(p) return ptrType(y, p);
    return y;
}

cell *getTypeList(tocon *con) {
    cell *l;
    int lo = con->line;
    int y = getType(con);
    if(y == TNOT) return 0;
    return &cells[symCell(LTYPE, y, lo)];
}

int sameType(datatype *d) {
    int i, j;
    datatype *t;
    for(i = 0; (t = &types[i]) != d; i++) {
        if(t->t != d->t) continue;
        if(d->t >= TARR && d->t <= TUNION) {
            if(d->n != t->n) continue;
            for(j = 0; j < d->n && d->e[j] == t->e[j]; j++);
            if(j >= d->n) return i;
        } else if(d->t == TPTR) {
            if(d->p == t->p && d->i == t->i) return i;
        } else return i;
    }
    return -1;
}

int funType(tocon *con, int rt) {
    datatype *d;
    int i, i0 = con->line;
    int n = countEl(con);
    d = &types[ntypes++];
    d->t = TFUN;
    d->e = &els[nels+1];
    d->n = n;
    nels += n+1;
    inccon(con);
    for(i = 0; i < n; i++) {
        d->e[i] = getType(con);
        inccon(con);
    }
    inccon(con);
    d->e--; d->e[0] = rt; d->n++;
    if((i = sameType(d)) != -1) { ntypes--; nels -= n+1; return i; }
    return (int)(d-types);
}

short ex0(tocon *con) {
    static short com[] = { COM,-1, };
    static short ass[] = { PE,ME,ANDE,ORE,XORE,MULE,DIVE,REME,SHLE,SHRE,-1, };
    static short *ops[] = { com, ass, 0, };
    cell *l;
    if(*symbol(con->tokens[con->i]) == '#')
        return readMacro(con);
    if(l = getTypeList(con))
        return l-cells;
    return ex(con, ops, tern);
}

short listBrace(tocon *con) {
    short l0 = con->line;
    if(nextSym(con) != LP) {
        errr(l0); printf("expected (\n"); exit(1);
    }
    short c = ex0(con);
    if(nextSym(con) != RP) {
        errr(l0); printf("expected )\n"); exit(1);
    }
    return c;
}

short semi(tocon *con) {
    if(nextSym(con) != SEMI) {
        errr(con->line-con->offs[con->i-1]); printf("expected ;\n"); exit(1);
    }
}

short listBlock(tocon *con) {
    short c, a, b;
    c = nextSymCell(con);
    switch(cells[c].sym) {
    case IF:
        a = listBrace(con);
        b = listBlock(con);
        if(con->tokens[con->i] == ELSE)
            return cons(c, cons(a, cons(b, listBlock(con))));
        return cons(c, cons(a, b));
        break;
    case WHILE:
        a = listBrace(con);
        b = listBlock(con);
        return cons(c, cons(a, cons(b, 0)));
    case LC:
        c = cons(c,0);
        while(con->tokens[con->i] != RC)
            append(c, listBlock(con));
        inccon(con);
        return c;
    case COL:
        return 0;
    case RETURN:
        c = cons(c, cons(ex0(con), 0));
        semi(con);
        return c;
    default:
        con->line -= con->offs[--con->i];
        c = ex0(con);
        semi(con);
        return cons(symCell(LSYM,EX,cells[c].lo),cons(c,0));
    }
}

short listOuter(tocon *con) {
    short t, name, c;
    if(symbol(con->tokens[con->i])[0] == '#')
        return readMacro(con);
    t = getType(con);
    if(t == TNOT) {
        errh(startline);
        printf("unknown type %s\n", symbol(con->tokens[con->i])); exit(1);
    }
    name = con->tokens[con->i];
    inccon(con);
    //t = dimType(con, t);
    if(con->tokens[con->i] == LP) {
        t = funType(con, t);
        return cons(symCell(LSYM,DEFUN,startline),
                 cons(symCell(LSYM,name,startline),
                   cons(symCell(LTYPE,t,startline),
                     cons(listBlock(con),0))));
    }
    return 0;
}

short nextBlock(FILE *fp) {
    short tokens[MAXTOKENS];
    unsigned char offs[MAXTOKENS];
    char buf[BUFSZ];
    int ntokens = parseTokens(fp, buf, tokens, offs);
    int l = startline;
    if(!ntokens) return 0;
    int i;
    printf("%d ", startline);
    for(i = 0; i < ntokens; i++) {
        if(offs[i]) printf("\n%d ", l+=offs[i]);
        printf("%s ", symbol(tokens[i]));
    }
    printf("\n\n");
    tocon con;
    con.i = 0; con.tokens = tokens; con.offs = offs;
    con.line = 0;
    ncells = 1;
    return listOuter(&con);
}

void printCell(short c) {
    int i, o;
    if(!c) { printf("NIL"); return; }
    cell *l = &cells[c];
    switch(l->t) {
    case LSYM:
        printf("%s", symbol(l->sym)); break;
    case LTYPE:
        printf("TYPE#%d", l->sym); break;
    case LINT:
        printf("%d", l->n); break;
    case LSTR:
        printf("%s\"", symbol(l->sym)); break;
    case LLIST:
        printf("("); printCell(l->car);
        o = l->lo;
        while(l->cdr) {
            l = &cells[l->cdr];
            printf(" ");
            if(l->t != LLIST) { printf(". "); printCell(l-cells); break; }
            printCell(l->car);
        }
        printf(")");
        break;
    }
}

void printType(short t) {
    static char *primstrs[] = {
        "void", "char", "short", "int", "long",
        "uchar", "ushort", "uint", "ulong",
    };
    datatype *d;
    int i;
    if(t < 0) { printf("%s", primstrs[t-TVOID]); return; }
    if((d=&types[t])->t == TPTR) {
        printType(d->i);
        for(i = 0; i < d->p; i++) printf("*");
        return;
    }
    printf("<");
    if(d->t == TARR) {
        printType(d->i);
        for(i = 0; i < d->n; i++) {
            printf("["); if(d->e[i]) printType(d->e[i]); printf("]");
        }
    } else if(d->t == TFUN) {
        printf("fun");
        for(i = 1; i < d->n; i++) {
            printf(" "); printType(d->e[i]);
        }
        printf(" : "); printType(d->e[0]);
    } else {
        if(d->t == TSTRUCT) printf("struct");
        else printf("union");
        for(i = 0; i < d->n; i++) {
            printf(" "); printType(d->e[i]);
        }
    }
    printf(">");
}

void printTypes() {
    int i;
    for(i = 0; i < ntypes; i++) {
        printf("TYPE#%d: ", i); printType(i); printf("\n");
    }
    printf("\n");
}

/* COMPILER */

#define TEXTSZ 32768
#define DATASZ 65536
#define MAXREFS 8192
#define MAXGLOBALS 4096
#define MAXLOCALS 50

char text[TEXTSZ];
int ntext = 0;
char data[DATASZ];
int ndata = 0;

struct { short sym, t; int a; } globals[MAXGLOBALS];
int nglobals = 0;

struct { int r; unsigned short g; } refs[MAXREFS];
int nrefs = 0;

int nlocals;
struct { short sym, t; int a; } locals[MAXLOCALS];
int localmax;

int findGlobal(short sym) {
    int i;
    for(i = 0; i < nglobals; i++)
        if(globals[i].sym == sym) return i;
    return -1;
}

int findLocal(short sym) {
    int i;
    for(i = nlocals-1; i >= 0; i--)
        if(locals[i].sym == sym) return i;
    return -1;
}

int typeSize(short t);

int arrSize(datatype *d) {
    int t = 1;
    for(int i = 0; i < d->n; i++) t *= d->e[i];
    if(!t) return 4;
    return typeSize(d->i)*t;
}

int typeSize(short t) {
    switch(types[t].t) {
    case TPTR: return 4;
    case TARR: return arrSize(&types[t]);
    case TLONG: case TULONG: return 8;
    case TINT: case TUINT: return 4;
    case TSHORT: case TUSHORT: return 2;
    case TCHAR: case TUCHAR: return 1;
    default: return 4;
    }
}

void addExLocals(short c) {
    int a;
    if(c && cells[c].t == LLIST) {
        if(cells[a=car(c)].t == LTYPE) {
            cells[c].sym = cells[car(cdr(c))].sym;
            cells[c].t = LSYM;
            locals[nlocals].a = nlocals ?
                locals[nlocals-1].a + typeSize(locals[nlocals-1].t) : 0;
            locals[nlocals].t = cells[a].sym;
            locals[nlocals].sym = cells[c].sym;
            if((a = locals[nlocals].a + typeSize(locals[nlocals].t)) > localmax)
                localmax = a;
            nlocals++;
        } else { addExLocals(car(c)); addExLocals(cdr(c)); }
    }
}

void addBlockLocals(short c) {
    while(c) {
        if(cells[car(car(c))].sym == EX)
            addExLocals(cdr(car(c)));
        c = cdr(c);
    }
}

void compileBlock(short c) {
    addBlockLocals(c);
}

void compileEx(short c) {
}

void compileStatement(short c) {
    short oldlocals = nlocals;
    switch(cellsym(car(c))) {
    case LB:
        compileBlock(cdr(c));
        nlocals = oldlocals;
        break;
    case EX:
        if(cdr(cdr(c))) {
            addExLocals(c=cdr(c));
            compileEx(c);
            nlocals = oldlocals;
        }
        break;
    }
}

void compileFun(short c) {
    short name = cellsym(car(c));
    short type = cellsym(car(cdr(c)));
    localmax = nlocals = 0;
    int i = findGlobal(name);
    if(i == -1) {
        i = nglobals++; globals[i].sym = name; globals[i].t = type;
    } else if(globals[i].a) {
        errr(cello(car(c))); printf("%s already defined\n", symbol(name));
        exit(1);
    } else if(globals[i].t != type) {
        errr(cello(car(c)));
        printf("conflicting types for %s\n", symbol(name)); exit(1);
    }
    globals[i].a = ntext;
    compileStatement(cdr(cdr(c)));
}

void compileOuter(short c) {
    switch(cellsym(car(c))) {
    case DEFUN:
        compileFun(cdr(c));
        break;
    }
}

void compileFile(char *filename) {
    FILE *fp = openFile(filename, "r");
    short c;
    lineno = 1; infile = filename;
    while(c = nextBlock(fp)) { printCell(c); printf("\n\n"); }
    fclose(fp);
    printTypes();
}

void saveFile(char *filename) {
}

/* MAIN */

char *outfile = "a.out";

void option(char *s) {
    switch(*(s++)) {
    case 'o': outfile = s; break;
    default: printf("unknown option '%c'\n", *(s-1)); break;
    }
}

int main(int argc, char **args) {
    int i, j;
    for(i = 1; i < argc; i++)
        if(*args[i] == '-') { option(args[i]+1); argc--;
            for(j = i--; j < argc; j++) args[j] = args[j+1];
        }
    if(argc == 1) return 1;
    for(i = 1; i < argc; i++) compileFile(args[i]);
    saveFile(outfile);
    return 0;
}

