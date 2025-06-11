#include <stdio.h>

#include "tree.h"

// These constants are for drawing svg diagrams
#define BOX_WIDTH 80
#define BOX_HEIGHT 20
#define HSEP 10
#define VSEP 10
#define FONT_SIZE 8

#define BUF_SIZE (1024 * 1024 * 8) // 8 Mb buffer

struct Point {
    int x;
    int y;
};

struct DiagObj {
    int id;
    struct Point pos;
};

static int _obj_id = 0;
static int _file_id = 0;

static int _get_next_obj_id();
static int _get_next_file_id();
static struct Point _make_point(int x, int y);
static struct DiagObj _make_obj(struct Point pos);
static void _merge_trees(struct Node* tree0, struct Node* tree1);
static struct Node* _recycle(struct Tree* tree);

static void _svg_template(char* file_buf, char* content_buf, int w, int h);
static void _node_template(char* buf, uintptr_t value, struct DiagObj obj,
    bool_t is_root, bool_t is_indir, bool_t is_reduced);
static void _path_template(char* buf, struct DiagObj obj0, struct DiagObj obj1);
static int _get_tree_width(struct Node* tree);
static int _get_tree_height(struct Node* tree);
static void _draw_tree(char* node_buf, char* path_buf, uintptr_t node,
    struct DiagObj obj, bool_t is_root);

static void _pretty_print_subtree(struct Node* node);

// -----------------------------------------------------------------------------

struct Tree* tree_make() {
    struct Tree* tree = malloc(sizeof(struct Tree));
    tree->nodes = node_stack_make();
    tree->root = NULL;
    tree->freelist = freelist_stack_make();
    return tree;
}

// Allocate space for a new node on the stack
struct Node* alloc_node(struct Tree* tree)
{
    struct Node* new_node = node_stack_alloc(tree->nodes);
    return new_node;
}

// Either allocate or reuse space for a new node, and fill it with data
struct Node* add_node(struct Tree* tree, const struct Node* left,
    const struct Node* right)
{
    struct Node* new_node = _recycle(tree);

    if (new_node == NULL) {
        new_node = alloc_node(tree);
    }

    set_left_right(new_node, left, right);
    return new_node;
}

// Copy a node to another address, creating an indirection node if necessary
// At the end of the operation, `old_addr` should be equal to `*new_addr`
void duplicate_node_to(struct Tree* tree, struct Node* old_addr,
    struct Node** new_addr)
{
    // assert(old_addr != NULL);

    // Check if the node is a leaf
    if (old_addr == NULL) {
        *new_addr = NULL;
        return;
    }

    // Check if the node to be copied is already an indirection
    if (get_tag_left(old_addr) != Indir) {
        // If not, envelope it in one and update the old address
        struct Node* result = add_node(tree, get_left(old_addr),
            get_right(old_addr));
        set_indir_to(old_addr, result);
    }
    incr_ref(old_addr);

    // Update the new address
    // *new_addr = old_addr;
    set_value_at_node_addr(new_addr, old_addr);
    set_tag_right(*new_addr, Unreduced);
}

// When a subtree is deleted, its root is marked in the freelist stack
void delete_node(struct Tree* tree, struct Node* node_to_delete) {
    if (is_leaf(node_to_delete) == TRUE) {
        // Node is already just a leaf, nothing to do
        return;
    }

    if (get_tag_left(node_to_delete) == Indir) {
        if (is_zero_ref(node_to_delete) == FALSE) {
            decr_ref(node_to_delete);
        }
        return;
    }

    freelist_stack_push(tree->freelist, node_to_delete);
}

void print_empty(int ind, struct Tree* tree) {
    if (tree->freelist == NULL) {
        debug_indent(ind, "No free nodes.\n");
    } else {
        debug_indent(ind, "Free nodes:\n");
        freelist_stack_print(ind, tree->freelist);
    }
}

void print_root(int ind, struct Node* tree) {
    #ifdef DEBUG_PRINTS
    debug_indent(ind, "Eval root: %" PRIuPTR "\n", (uintptr_t)tree);
    #endif
}

void _pretty_print_subtree(struct Node* node) {
    if (is_leaf(node)) {
        printf("t");
        return;
    }

    _pretty_print_subtree(get_left(node));
    
    if (!is_leaf(get_right(node))) {
#ifndef USE_SPACES
            printf("(");
#else
            printf(" (");
    } else {
            printf(" ");
#endif
    }
    _pretty_print_subtree(get_right(node));
    if (!is_leaf(get_right(node))) {
        printf(")");
    }
}

// Print a textual representation of a tree (e.g. "ttt(tt)")
void pretty_print(struct Node* root) {
    _pretty_print_subtree(root);
    printf("\n");
}

void print_tree(int ind, struct Tree* tree) {
    #ifdef DEBUG_PRINTS
    node_stack_print(ind, tree->nodes);
    debug_indent(ind, "\n");
    print_empty(ind, tree);
    debug_indent(ind, "\n");
    print_root(ind, tree->root);
    debug_indent(ind, "Root addr: %" PRIuPTR "\n", (uintptr_t)&tree->root);
    #endif
}

void draw_tree(char* filename, struct Tree* tree) {
    #ifdef OUTPUT_DIAGRAMS
    char* buf0 = malloc(BUF_SIZE); // node_buf and file_buf
    buf0[0] = 0;
    char* buf1 = malloc(BUF_SIZE); // path_buf
    buf1[0] = 0;
    
    int w = _get_tree_width(tree->root);
    int h = _get_tree_height(tree->root);
    struct Node** data = (struct Node**)segment_get_data(
        tree->freelist->current_segment);
    struct Node** terminator = (struct Node**)segment_get_next_free_addr(
        tree->freelist->current_segment);
    while (data != terminator) {
        int w2 = _get_tree_width(*data);
        w = (w > w2 ? w : w2) + HSEP * 2;
        int h2 = _get_tree_height(*data);
        h = h + h2 + VSEP;
        data++;
    }
    w += VSEP * 2;
    h += HSEP * 2;
    data = (struct Node**)segment_get_data(tree->freelist->current_segment);
    
    // Tree nodes
    int start_y = VSEP + BOX_HEIGHT / 2;
    _draw_tree(buf0, buf1, (uintptr_t)tree->root,
        _make_obj(_make_point(w / 2, start_y)), TRUE);
    strcat(buf1, buf0);
    start_y += _get_tree_height(tree->root) + VSEP;

    // Freelist tree nodes
    // NOTE: lazy solution - only draw the most recent segment
    data = (struct Node**)segment_get_data(tree->freelist->current_segment);
    terminator = (struct Node**)segment_get_next_free_addr(
        tree->freelist->current_segment);
    while (data != terminator) {
        _draw_tree(buf0, buf1, (uintptr_t)*data,
            _make_obj(_make_point(w / 2, start_y)), TRUE);
        strcat(buf1, buf0);
        start_y += _get_tree_height(*data) + VSEP;
        data++;
    }

    _svg_template(buf0, buf1, w, h);
    char filename_numbered[256];
    sprintf(filename_numbered, "%s_%d.svg", filename, _get_next_file_id());
    FILE* fptr = fopen(filename_numbered, "w");
    printf("File written: %s\n", filename_numbered);
    fprintf(fptr, "%s", buf0);
    fclose(fptr);

    free(buf0);
    free(buf1);
    #endif
}

// ----------------------------- INTERNAL METHODS -----------------------------

static int _get_next_obj_id() {
    int result = _obj_id;
    _obj_id++;
    return result;
}

static int _get_next_file_id() {
    int result = _file_id;
    _file_id++;
    return result;
}

static struct Point _make_point(int x, int y) {
    struct Point point;
    point.x = x;
    point.y = y;
    return point;
}

static struct DiagObj _make_obj(struct Point pos) {
    struct DiagObj obj;
    obj.id = _get_next_obj_id();
    obj.pos.x = pos.x;
    obj.pos.y = pos.y;
    return obj;
}

static void _svg_template(char* file_buf, char* content_buf, int w, int h) {
    #ifdef OUTPUT_DIAGRAMS
    sprintf(file_buf, "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
"<svg\n"
"   width=\"%dpx\"\n"
"   height=\"%dpx\"\n"
"   version=\"1.1\"\n"
"   viewBox=\"0 0 %d %d\"\n"
"   id=\"svg4\"\n"
"   sodipodi:docname=\"drawing_inkscape.svg\"\n"
"   inkscape:version=\"1.3.2 (091e20ef0f, 2023-11-25)\"\n"
"   xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"\n"
"   xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"\n"
"   xmlns=\"http://www.w3.org/2000/svg\"\n"
"   xmlns:svg=\"http://www.w3.org/2000/svg\">\n"
"  <defs\n"
"     id=\"defs4\" />\n"
"  <sodipodi:namedview\n"
"     id=\"namedview4\"\n"
"     pagecolor=\"#505050\"\n"
"     bordercolor=\"#ffffff\"\n"
"     borderopacity=\"1\"\n"
"     inkscape:showpageshadow=\"0\"\n"
"     inkscape:pageopacity=\"0\"\n"
"     inkscape:pagecheckerboard=\"1\"\n"
"     inkscape:deskcolor=\"#505050\"\n"
"     inkscape:zoom=\"4.5877088\"\n"
"     inkscape:cx=\"93.837691\"\n"
"     inkscape:cy=\"124.78996\"\n"
"     inkscape:window-width=\"1920\"\n"
"     inkscape:window-height=\"1008\"\n"
"     inkscape:window-x=\"0\"\n"
"     inkscape:window-y=\"0\"\n"
"     inkscape:window-maximized=\"1\"\n"
"     inkscape:current-layer=\"svg4\" />\n"
"<!-- Background rect -->\n"
"  <rect\n"
"     x=\"0\"\n"
"     y=\"0\"\n"
"     width=\"%d\"\n"
"     height=\"%d\"\n"
"     fill=\"#202020\"\n"
"     id=\"background\" />\n"
"  %s\n"
"</svg>\n", w, h, w, h, w, h, content_buf);
    #endif
}

// `obj` is the midpoint of the box
static void _node_template(char* buf, uintptr_t value, struct DiagObj obj,
    bool_t is_root, bool_t is_indir, bool_t is_reduced)
{
    #ifdef OUTPUT_DIAGRAMS
    int left = obj.pos.x - BOX_WIDTH / 2;
    int top = obj.pos.y - BOX_HEIGHT / 2;
    int text_left = left + 5;
    int text_top = obj.pos.y + FONT_SIZE / 2;
    char rect_id[256];
    char text_id[256];
    char group_id[256];
    char rect_fill[16] = "#808080";
    char text[128];
    if (is_root) {
        sprintf(rect_fill, "#FFFFFF");
    }
    sprintf(rect_id, "rect%d", obj.id);
    sprintf(text_id, "text%d", obj.id);
    sprintf(group_id, "g%d", obj.id);
    
    int offset = sprintf(text, "%" PRIuPTR, value);
    if (is_indir == TRUE) {
        struct Node* temp = (struct Node*)untag_value(value);
        size_t refcount = temp->left;
        refcount >>= 1;
        offset += sprintf(text + offset, " (%lu)", refcount);
    }
    if (is_reduced == TRUE) {
        sprintf(text + offset, " *");
    }
    
    char* temp_buf = malloc(BUF_SIZE);
    sprintf(temp_buf, "<g\n"
"     id=\"%s\">\n"
"    <rect\n"
"       x=\"%d\"\n"
"       y=\"%d\"\n"
"       width=\"%d\"\n"
"       height=\"%d\"\n"
"       fill=\"%s\"\n"
"       id=\"%s\" />\n"
"    <text\n"
"       x=\"%d\"\n"
"       y=\"%d\"\n"
"       fill=\"#000000\"\n"
"       font-family=\"FreeSans\"\n"
"       font-size=\"%dpx\"\n"
"       xml:space=\"preserve\"\n"
"       id=\"%s\">%s</text>\n"
"  </g>\n", group_id, left, top, BOX_WIDTH, BOX_HEIGHT, rect_fill, rect_id,
        text_left, text_top, FONT_SIZE, text_id, text);
    strcat(buf, temp_buf);
    free(temp_buf);
    #endif
}

static void _path_template(char* buf, struct DiagObj obj0, struct DiagObj obj1)
{
    #ifdef OUTPUT_DIAGRAMS
    char rect_id_0[256];
    char rect_id_1[256];
    sprintf(rect_id_0, "rect%d", obj0.id);
    sprintf(rect_id_1, "rect%d", obj1.id);

    char* temp_buf = malloc(BUF_SIZE);
    sprintf(temp_buf, "<path\n"
"     style=\"fill:none;stroke:#808080;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1\"\n"
"     d=\"M %d,%d %d,%d\"\n"
"     inkscape:connector-type=\"polyline\"\n"
"     inkscape:connector-curvature=\"0\"\n"
"     inkscape:connection-start=\"#%s\"\n"
"     inkscape:connection-end=\"#%s\" />\n", obj0.pos.x, obj0.pos.y, obj1.pos.x,
        obj1.pos.y, rect_id_0, rect_id_1);
    strcat(buf, temp_buf);
    free(temp_buf);
    #endif
}

static int _get_tree_width(struct Node* tree) {
    if (tree == NULL) {
        return BOX_WIDTH;
    }

    return _get_tree_width(get_left(tree)) + _get_tree_width(get_right(tree))
        + HSEP;
}

static int _get_tree_height(struct Node* tree) {
    int height = BOX_HEIGHT;
    if (tree != NULL) {
        height += VSEP;
        if (get_tag_left(tree) == Indir) {
            height *= 2;
        }
        
        int left_height = _get_tree_height(get_left(tree));
        int right_height = _get_tree_height(get_right(tree));
        if (left_height < right_height) {
            height += right_height;
        } else {
            height += left_height;
        }
    }

    return height;
}

// Merge tree1 to the rightmost empty node of tree0
static void _merge_trees(struct Node* tree0, struct Node* tree1) {
    // Find the rightmost empty node's address
    struct Node* rightmost_empty_addr = tree0;
    while (get_right(rightmost_empty_addr) != NULL) {
        rightmost_empty_addr = get_right(rightmost_empty_addr);
    }

    // Attach the rest of the deleted nodes to this node
    set_right(rightmost_empty_addr, tree1);
}

// Take a tree from the freelist, break off its children, store them in the
// freelist, and return the root node's address
static struct Node* _recycle(struct Tree* tree) {
    struct Node* result = freelist_stack_pop(tree->freelist);
    if (result != NULL) {
        // Indirs are not allowed in the freelist
        assert(get_tag_left(result) != Indir);

        struct Node* left = get_left(result);
        if (left != NULL) {
            if (get_tag_left(left) == Indir) {
                if (is_zero_ref(left) == FALSE) {
                    decr_ref(left);
                }
            } else {
                freelist_stack_push(tree->freelist, left);
            }
        }

        struct Node* right = get_right(result);
        if (right != NULL) {
            if (get_tag_left(right) == Indir) {
                if (is_zero_ref(right) == FALSE) {
                    decr_ref(right);
                }
            } else {
                freelist_stack_push(tree->freelist, right);
            }
        }
    }
    return result;
}

static void _draw_tree(char* node_buf, char* path_buf, uintptr_t node,
    struct DiagObj obj, bool_t is_root)
{
    struct Node* node_addr = (struct Node*)untag_value(node);
    bool_t is_indir = FALSE;
    bool_t is_reduced = FALSE;
    if (node_addr != NULL) {
        if (get_tag_left(node_addr) == Indir) {
            is_indir = TRUE;
        }
        if (get_tag_right(node_addr) == Reduced) {
            is_reduced = TRUE;
        }
    }
    _node_template(node_buf, (uintptr_t)node, obj, is_root, is_indir,
        is_reduced);

    if (node_addr == NULL) {
        return;
    }

    if (is_indir == TRUE) {
        struct Point new_pos = _make_point(obj.pos.x,
            obj.pos.y + VSEP + BOX_HEIGHT);
        struct DiagObj new_obj = _make_obj(new_pos);

        _path_template(path_buf, obj, new_obj);
        _draw_tree(node_buf, path_buf, node_addr->right, new_obj, FALSE);
    } else {
        int left_width = _get_tree_width(get_left(node_addr));
        int right_width = _get_tree_width(get_right(node_addr));
        int full_width = left_width + HSEP + right_width;

        struct Point left_pos = _make_point(
            obj.pos.x - full_width / 2 + left_width / 2,
            obj.pos.y + VSEP + BOX_HEIGHT);

        struct Point right_pos = _make_point(
            obj.pos.x + full_width / 2 - right_width / 2,
            obj.pos.y + VSEP + BOX_HEIGHT);

        struct DiagObj left_obj = _make_obj(left_pos);
        struct DiagObj right_obj = _make_obj(right_pos);

        _path_template(path_buf, obj, left_obj);
        _path_template(path_buf, obj, right_obj);
        _draw_tree(node_buf, path_buf, node_addr->left, left_obj, FALSE);
        _draw_tree(node_buf, path_buf, node_addr->right, right_obj, FALSE);
    }
}
