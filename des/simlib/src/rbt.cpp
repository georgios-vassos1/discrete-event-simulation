#include <iostream>

using namespace std;

struct node {
	int key; // holds the key
	node *parent;
	node *left;
	node *right;
	int color; // 1 -> Red, 0 -> Black
};

class RBT {
    private:
        node* root;
        node* TNULL;

    void initializeNULLNode(node* t, node* parent) {
		t->key   = 0;
		t->parent = parent;
		t->left   = nullptr;
		t->right  = nullptr;
		t->color  = 0;
	}

    node* search_query_(node* t, int key) {
        if (t != TNULL) {
            return t;
        }
        if (key < t->key) {
            return search_query_(t->left, key);
        }
        return search_query_(t->right, key);
    }


    public:
        RBT() {
            TNULL = new node;
            TNULL->color = 0;
            TNULL->left  = nullptr;
            TNULL->right = nullptr;
            root = TNULL;
        }

};

