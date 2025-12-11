#include "test.h"

#include "../src/vm.h"
#include <ctype.h>
#include <string.h>

size_t parse_expr(struct Tree* tree, const char **p);

void parse_whitespace(const char **p) {
    while (isspace((unsigned char)**p)) {
        (*p)++;
    }
}

// Parse a single atom: either 't' or '(' expr ')'
size_t parse_atom(struct Tree* tree, const char **p) {
    parse_whitespace(p);

    if (**p == 't') {
        (*p)++;
        return 0; // leaf
    } else if (**p == '(') {
        (*p)++;
        int idx = parse_expr(tree, p);
        if (**p != ')') {
            fprintf(stderr, "Error: expected ')'\n");
            exit(EXIT_FAILURE);
        }
        (*p)++;
        return idx;
    } else {
        fprintf(stderr, "Unexpected char: %c\n", **p);
        exit(EXIT_FAILURE);
    }
}

// Parse left-associative applications
size_t parse_expr(struct Tree* tree, const char **p) {
    size_t left = parse_atom(tree, p);
    while (1) {
        const char* save = *p;
        parse_whitespace(p);
        if (**p == 't' || **p == '(') {
            size_t right = parse_atom(tree, p);
            Node node = node_make(NODE_TYPE_APP, 1, left, right);
            Index new_left = tree_add_node(tree, node);
            left = new_left;
        } else {
            *p = save;
            break;
        }
    }
    return left;
}

struct Vm vm_parse(const char* str) {
    struct Vm vm = vm_make(vm_default_config);
    size_t last_index = parse_expr(&vm.tree, &str);
    printf("Last index: %lu\n", last_index);
    tree_debug_print(vm.tree);
    tree_set_node(vm.tree, 0, node_make(NODE_TYPE_INDIR, 1, 0, last_index));
    tree_debug_print(vm.tree);
    printf("\n\n\n");
    return vm;
}

void check_eval(const char* src, const char* expected) {
    struct Vm vm = vm_parse(src);
    vm_run(&vm);
    char buffer[65536];
    tree_print(vm.tree, buffer, FALSE);
    bool_t result = (strcmp(buffer, expected) == 0) ? TRUE : FALSE;
    if (result == FALSE) {
        printf("Result: %s, expected: %s\n", buffer, expected);
        tree_debug_print(vm.tree);
    }
    sprintf(buffer, "%s -> %s", src, expected);
    check(buffer, result);
}

void test() {
    printf("Simple trees\n");
    check_eval("t",     "t");
    check_eval("tt",    "tt");
    check_eval("ttt",   "ttt");

    printf("Reduction rules\n");
    check_eval("tttt",          "t");
    check_eval("t(tt)tt",       "tt(tt)");
    check_eval("t(ttt)tt",      "t");
    check_eval("t(ttt)t(tt)",   "tt");
    check_eval("t(ttt)t(ttt)",  "ttt");

    printf("Redexes on lower levels\n");
    check_eval("t(tt(tttt))tt", "t");
    check_eval("t(tt(tttt))t",  "t(ttt)t");

    printf("All reduction rules in lower levels\n");
    check_eval("t(tttt)",                       "tt");
    check_eval("t(t(tt)tt)",                    "t(tt(tt))");
    check_eval("t(t(ttt)tt)",                   "tt");
    check_eval("t(t(ttt)t(tt))",                "t(tt)");
    check_eval("t(t(ttt)t(ttt))",               "t(ttt)");
    check_eval("t(tttt)(t(ttt)tt)(t(tt)tt)",    "t");
    check_eval("t(t(tttt)(tttt)(tttt))(t(tttt)(tttt)(tttt))(t(tttt)(tttt)(tttt))",
        "t");
    check_eval("t (tttt) (t(tt)tt) (t(ttt)tt) (t(ttt)t(tt)) (t(ttt)t(ttt))",
        "tt(ttt)");
    check_eval("t (t (tttt) (t(tt)tt) (t(ttt)tt) (t(ttt)t(tt)) (t(ttt)t(ttt)))",
        "t(tt(ttt))");

    printf("All rules chained after each other (3, 2, 2, 5, 2, 4, 1)\n");
    check_eval("t(t(ttt)tt)(t(tt)t(t(t(ttt)))tt)t", "tt");

    printf("All rules, smaller version (2, 2, 2, 2, 5, 4, 2, 1, 3)\n");
    check_eval("t (t (t (t (t (t t) t)))) t (t (t t t)) t", "tt");

    printf("All rules chained after each other exactly once (2, 5, 4, 3, 1)\n");
    check_eval("tt(t(t(t(ttt)))(tt)(t(ttt))tt)t", "t");

    printf("Some random trees\n");
    check_eval("t (tttt) (tttt) (tttt)",    "t");
    check_eval("tt (ttt (tt)) t (ttt)" ,    "t(ttt)");
    check_eval("t(tttt)t"              ,    "ttt");
    check_eval("tt(tttt)"              ,    "ttt");
    check_eval("t (tt (tttt)) t (tttt)",    "t");

    printf("Test if children are reduced properly\n");
    check_eval("ttttttt", "t");

    printf("And True False\n");
    check_eval("(t (t (t t t) (t t (t (t (t t)) t))) t) (t t) t", "t");

    printf("And True True\n");
    check_eval("(t (t (t t t) (t t (t (t (t t)) t))) t) (t t) (t t)", "tt");

    printf("Not (And True False)\n");
    check_eval("(t (t (t t) (t t t)) t) ((t (t (t t t) (t t (t (t (t t)) t))) t) (t t) t)",
        "tt");

    printf("Equal t t\n");
    check_eval("(t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t (t (t t (t (t (t t t)) (t (t (t (t t) (t t t)) (t t (t t t))))))) (t (t (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t t))) (t (t (t t t)) t)))) (t t (t t))))) (t (t (t t (t (t (t t t)) (t t))))))) (t t (t t (t t t)))))) (t (t (t t (t (t (t t (t (t (t t (t (t t (t t t))))))))))) (t (t (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t t))) (t (t (t t t)) t)))) (t t (t t))))) (t (t (t t (t (t (t t (t (t (t t (t (t (t t t)) t))) (t t))))))) (t (t (t t (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t t))) (t (t (t t t)) t)))) (t t (t t)))))))) (t (t (t t (t (t (t t (t (t (t t (t (t (t t (t (t (t t t)) t))) (t t)))))))))) (t (t (t t (t (t (t t (t (t (t t t) (t t (t (t (t t)) t))) t)))))))))))) (t (t (t t)) t)))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))) t t",
        "tt");

    printf("double 2\n");
    check_eval("(t (t (t (t (t t (t (t t t)))) (t (t (t t (t t))) (t (t (t t (t t))) (t t))))) (t (t (t t)) t)) (t t (t (t t) t))",
        "tt(tt(t(tt)t))");

    printf("A collection of trees that didn't reduce correctly in earlier versions\n");
    check_eval("t(t(t(tt)))t(ttt)", "t(t(ttt))t");
    check_eval("t(t(ttt)tt)(tt)",   "tt(tt)");
    check_eval("(t (t t) (t t)) (t( t t) (t t))", "t(t(tt)(tt))(tt(t(tt)(tt)))");
    check_eval("(t (t t) t) (t( t t) t)", "t(t(tt)t)(t(t(tt)t))");

    check_eval("(t (t (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t (t (t t) t) t)))) (t (t (t (t (t t (t (t (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t t))) (t (t (t t t)) t)))) (t t (t t))))) (t (t (t t (t (t (t t (t (t (t (t (t t (t (t (t t t))))) t))))) (t t)))) (t t)))) (t t t)))) (t (t (t t (t t)))))) (t t (t (t t))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))) (t t t))))))) (t (t (t t (t (t (t t (t (t (t (t (t t (t (t (t t (t (t (t t t)) t))) (t t)))) (t (t (t t (t (t (t t (t (t (t (t (t t)) t))))) (t t)))) (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))))) (t t)))) (t (t (t (t (t t (t (t (t t (t (t (t t t)) t))) (t t)))) (t (t (t t t)) t))) (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t t))) (t (t (t t t)) t)))) (t t (t t))))) (t (t (t (t (t t (t (t (t t t)) t))) (t t))) (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))))))) (t t (t (t (t t) (t t t)) (t t (t t t))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))) (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))) (t t t)) (t t (t t (t (t t) t)))", "t(tt)(tt(t(tt)t))"); // fib 4 -> 5

    check_eval("(t (t (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t (t (t t) t) t)))) (t (t (t (t (t t (t (t (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t t))) (t (t (t t t)) t)))) (t t (t t))))) (t (t (t t (t (t (t t (t (t (t (t (t t (t (t (t t t))))) t))))) (t t)))) (t t)))) (t t t)))) (t (t (t t (t t)))))) (t t (t (t t))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))) (t t t))))))) (t (t (t t (t (t (t t (t (t (t (t (t t (t (t (t t (t (t (t t t)) t))) (t t)))) (t (t (t t (t (t (t t (t (t (t (t (t t)) t))))) (t t)))) (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))))) (t t)))) (t (t (t (t (t t (t (t (t t (t (t (t t t)) t))) (t t)))) (t (t (t t t)) t))) (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t t))) (t (t (t t t)) t)))) (t t (t t))))) (t (t (t (t (t t (t (t (t t t)) t))) (t t))) (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))))))) (t t (t (t (t t) (t t t)) (t t (t t t))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))) (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))) (t t t)) (t (t t) (t t (t (t t) t)))", "tt(tt(tt(t(tt)t)))");

    check_eval("(t (t (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t (t (t t) t) t)))) (t (t (t (t (t t (t (t (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t t))) (t (t (t t t)) t)))) (t t (t t))))) (t (t (t t (t (t (t t (t (t (t (t (t t (t (t (t t t))))) t))))) (t t)))) (t t)))) (t t t)))) (t (t (t t (t t)))))) (t t (t (t t))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))) (t t t))))))) (t (t (t t (t (t (t t (t (t (t (t (t t (t (t (t t (t (t (t t t)) t))) (t t)))) (t (t (t t (t (t (t t (t (t (t (t (t t)) t))))) (t t)))) (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))))) (t t)))) (t (t (t (t (t t (t (t (t t (t (t (t t t)) t))) (t t)))) (t (t (t t t)) t))) (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t t))) (t (t (t t t)) t)))) (t t (t t))))) (t (t (t (t (t t (t (t (t t t)) t))) (t t))) (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))))))) (t t (t (t (t t) (t t t)) (t t (t t t))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))) (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))) (t t t)) (t t (t t (t (t t) (t t (t (t t) t)))))", "tt(t(tt)(tt(tt(tt(tt(t(tt)(t(tt)(tt(t(tt)(tt(t(tt)(tt(t(tt)t)))))))))))))");
}
