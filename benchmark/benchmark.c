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
            Index new_left = tree_add_node(tree, NODE_TAG_APP);
            tree_change_child(tree, new_left, CHILD_SIDE_LEFT, left);
            tree_change_child(tree, new_left, CHILD_SIDE_RIGHT, right);
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
    tree_change_child(&vm.tree, 0, CHILD_SIDE_LEFT, last_index);
    return vm;
}

void benchmark(const char* src) {
    struct Vm vm = vm_parse(src);
    vm_init(&vm);
    vm_run(&vm);
    char buffer[65536];
    tree_print(vm.tree, buffer, TRUE);
    printf("Result: %s\n", buffer);
    // tree_debug_print(vm.tree);
}

int main() {
    // fib 28
    // benchmark("(t (t (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t (t (t t) t) t)))) (t (t (t (t (t t (t (t (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t t))) (t (t (t t t)) t)))) (t t (t t))))) (t (t (t t (t (t (t t (t (t (t (t (t t (t (t (t t t))))) t))))) (t t)))) (t t)))) (t t t)))) (t (t (t t (t t)))))) (t t (t (t t))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))) (t t t))))))) (t (t (t t (t (t (t t (t (t (t (t (t t (t (t (t t (t (t (t t t)) t))) (t t)))) (t (t (t t (t (t (t t (t (t (t (t (t t)) t))))) (t t)))) (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))))) (t t)))) (t (t (t (t (t t (t (t (t t (t (t (t t t)) t))) (t t)))) (t (t (t t t)) t))) (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t t))) (t (t (t t t)) t)))) (t t (t t))))) (t (t (t (t (t t (t (t (t t t)) t))) (t t))) (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))))))) (t t (t (t (t t) (t t t)) (t t (t t t))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))) (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))) (t t t)) (t t (t t (t (t t) (t (t t) (t (t t) t)))))");

    // fib 5
    // benchmark("(t (t (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t (t (t t) t) t)))) (t (t (t (t (t t (t (t (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t t))) (t (t (t t t)) t)))) (t t (t t))))) (t (t (t t (t (t (t t (t (t (t (t (t t (t (t (t t t))))) t))))) (t t)))) (t t)))) (t t t)))) (t (t (t t (t t)))))) (t t (t (t t))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))) (t t t))))))) (t (t (t t (t (t (t t (t (t (t (t (t t (t (t (t t (t (t (t t t)) t))) (t t)))) (t (t (t t (t (t (t t (t (t (t (t (t t)) t))))) (t t)))) (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))))) (t t)))) (t (t (t (t (t t (t (t (t t (t (t (t t t)) t))) (t t)))) (t (t (t t t)) t))) (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t t))) (t (t (t t t)) t)))) (t t (t t))))) (t (t (t (t (t t (t (t (t t t)) t))) (t t))) (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))))))) (t t (t (t (t t) (t t t)) (t t (t t t))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))) (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))) (t t t)) (t (t t) (t t (t (t t) t)))");

    // fib 16
    // benchmark("(t (t (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t (t (t t) t) t)))) (t (t (t (t (t t (t (t (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t t))) (t (t (t t t)) t)))) (t t (t t))))) (t (t (t t (t (t (t t (t (t (t (t (t t (t (t (t t t))))) t))))) (t t)))) (t t)))) (t t t)))) (t (t (t t (t t)))))) (t t (t (t t))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))) (t t t))))))) (t (t (t t (t (t (t t (t (t (t (t (t t (t (t (t t (t (t (t t t)) t))) (t t)))) (t (t (t t (t (t (t t (t (t (t (t (t t)) t))))) (t t)))) (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))))) (t t)))) (t (t (t (t (t t (t (t (t t (t (t (t t t)) t))) (t t)))) (t (t (t t t)) t))) (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t t))) (t (t (t t t)) t)))) (t t (t t))))) (t (t (t (t (t t (t (t (t t t)) t))) (t t))) (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))))))) (t t (t (t (t t) (t t t)) (t t (t t t))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))) (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))) (t t t)) (t t (t t (t t(t t(t (t t) t)))))");

    // fib 17
    benchmark("(t (t (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t (t (t t) t) t)))) (t (t (t (t (t t (t (t (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t t))) (t (t (t t t)) t)))) (t t (t t))))) (t (t (t t (t (t (t t (t (t (t (t (t t (t (t (t t t))))) t))))) (t t)))) (t t)))) (t t t)))) (t (t (t t (t t)))))) (t t (t (t t))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))) (t t t))))))) (t (t (t t (t (t (t t (t (t (t (t (t t (t (t (t t (t (t (t t t)) t))) (t t)))) (t (t (t t (t (t (t t (t (t (t (t (t t)) t))))) (t t)))) (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))))) (t t)))) (t (t (t (t (t t (t (t (t t (t (t (t t t)) t))) (t t)))) (t (t (t t t)) t))) (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t t))) (t (t (t t t)) t)))) (t t (t t))))) (t (t (t (t (t t (t (t (t t t)) t))) (t t))) (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))))))) (t t (t (t (t t) (t t t)) (t t (t t t))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))) (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))) (t t t)) (t (t t) (t t (t t (t t( t (t t) t)))))");
    // benchmark("(t (t (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t (t (t t) t) t)))) (t (t (t (t (t t (t (t (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t t))) (t (t (t t t)) t)))) (t t (t t))))) (t (t (t t (t (t (t t (t (t (t (t (t t (t (t (t t t))))) t))))) (t t)))) (t t)))) (t t t)))) (t (t (t t (t t)))))) (t t (t (t t))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))) (t t t))))))) (t (t (t t (t (t (t t (t (t (t (t (t t (t (t (t t (t (t (t t t)) t))) (t t)))) (t (t (t t (t (t (t t (t (t (t (t (t t)) t))))) (t t)))) (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))))) (t t)))) (t (t (t (t (t t (t (t (t t (t (t (t t t)) t))) (t t)))) (t (t (t t t)) t))) (t (t (t t (t (t (t (t (t t (t (t (t t t)) t))) (t (t (t t (t t))) (t (t (t t t)) t)))) (t t (t t))))) (t (t (t (t (t t (t (t (t t t)) t))) (t t))) (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))))))))) (t t (t (t (t t) (t t t)) (t t (t t t))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))) (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t))) (t t (t (t (t t (t (t (t t (t (t t t)))) (t (t (t t (t (t (t (t (t (t (t t (t (t (t t t))))) t)) (t t (t t (t (t t t) (t (t (t t (t (t (t t (t t)))))) t)))))) (t t t)))) (t (t (t t (t (t t))))))))) (t (t (t t (t (t (t (t (t t (t (t (t (t (t (t (t t)) t)) (t (t (t t)) t)))))) (t t)))))) (t t))))))) (t t t))");
}
