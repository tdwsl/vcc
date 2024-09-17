
void puts(char *s) {
    while(*s) sys(1, *(s++));
    //int i;
    //for(i = 0; s[i]; i++) sys(1, s[i]);
}

int main() {
    puts("Hello, world!\n");
    return 0;
}
