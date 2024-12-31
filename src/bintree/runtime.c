#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#define APP_STACK_SIZE 1024

// ----- FORWARD DECLS -----

void init_program();

// ----- END OF FORWARD DECLS -----

// The tree is stored as a series of Apps and Nodes. So FLL would be stored as
//     A
//    / \
//   A   L
//  / \
// L   L

// An App slot is a 128-bit integer value. The first 64 bits point to the first
// child, and the other 64 bits point to the second child. If a pointer is NULL,
// that child is a Node, otherwise it is an App.

// The App stack consists of App slots.
// There is also a distinct marker, pointing to the first empty slot, and
// another one, pointing to the last. This is to manage free space efficiently.
// Whenever an App slot is deleted, and it is not on the top of the stack, the
// last empty space (pointed to by the end marker) is updated to point to the
// newly deleted slot, and then the end marker is updated to point to this slot
// as well. That way, eventually each empty slot would point to the next one,
// forming a singly linked list. Adding and removing elements to it are constant
// operations.

typedef struct App {
    uint64_t app0;
    uint64_t app1;
};

struct AppStk {
    struct App* next_free_addr;
    struct App* last_free_addr;
    struct AppStk* next_block;
    struct AppStk* prev_block;
    struct App apps[APP_STACK_SIZE];
};

struct AppStk* current_block;

struct App* first_free_slot;
struct App* last_free_slot;

void init_app_stack() {
    struct AppStk* block = make_block();

    current_block = block;

    first_free_slot = NULL;
    last_free_slot = NULL;
}

struct AppStk* make_block() {
    struct AppStk* block = malloc(sizeof(struct AppStk));
    block->next_free_addr = block->apps;
    block->last_free_addr = block->apps + APP_STACK_SIZE;
    block->next_block = NULL;
    block->prev_block = NULL;
    return block;
}

void insert_new_block() {
    struct AppStk* block = make_block();
    current_block->next_block = block;
    block->prev_block = current_block;
    current_block = current_block->next_block;
}

struct App* add_app(struct App* app0, struct App* app1) {
    // Add app slot
    struct App* slot;
    if (first_free_slot != NULL) {
        slot = first_free_slot;
        first_free_slot = first_free_slot->app0;
    } else {
        slot = current_block->next_free_addr;
        if (current_block->next_free_addr == current_block->last_free_addr) {
            if (current_block->next_block == NULL) {
                insert_new_block();
            } else {
                current_block = current_block->next_block;
            }
        } else {
            current_block->next_free_addr++;
        }
    }
    slot->app0 = app0;
    slot->app1 = app1;
}

void delete_app(struct App* slot) {
    // TODO Delete app slot
    if (current_block->next_free_addr == slot) {
        if (current_block->next_free_addr != current_block->apps) {
            current_block->last_free_addr--;
        } else {
            current_block = current_block->prev_block;
        }
    } else {
        if (last_free_slot == NULL) {
            last_free_slot = slot;
        } else {
            last_free_slot->app0 = slot;
            last_free_slot = slot;
        }
        slot->app0 = NULL;
        slot->app1 = NULL;
    }
}

uint8_t* reduce() {
    // TODO
}

int main() {
    init_app_stack();
    init_program();
    reduce();
    return 0;
}

