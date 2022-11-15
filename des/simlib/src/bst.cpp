#include <Rcpp.h>
using namespace Rcpp;

class BST {

  struct node {
    double data;
    int    key;
    node*  left;
    node*  right;
  };

  node* root;

  node* makeEmpty(node* t) {
    if (t == NULL)
      return NULL;
    {
      makeEmpty(t->left);
      makeEmpty(t->right);
      delete t;
    }
    return NULL;
  }

  node* insert_(double x, int k, node* t)
  {
    if(t == NULL)
    {
      t = new node;
      t->data = x;
      t->key  = k;
      t->left = t->right = NULL;
    }
    else if(x < t->data)
      t->left = insert_(x, k, t->left);
    else if(x > t->data)
      t->right = insert_(x, k, t->right);
    return t;
  }

  node* findMin(node* t)
  {
    if(t == NULL)
      return NULL;
    else if(t->left == NULL)
      return t;
    else
      return findMin(t->left);
  }

  node* findMax(node* t) {
    if(t == NULL)
      return NULL;
    else if(t->right == NULL)
      return t;
    else
      return findMax(t->right);
  }

  node* remove_(double x, node* t) {
    node* temp;
    if(t == NULL)
      return NULL;
    else if(x < t->data)
      t->left = remove_(x, t->left);
    else if(x > t->data)
      t->right = remove_(x, t->right);
    else if(t->left && t->right)
    {
      temp = findMin(t->right);
      t->data = temp->data;
      t->right = remove_(t->data, t->right);
    }
    else
    {
      temp = t;
      if(t->left == NULL)
        t = t->right;
      else if(t->right == NULL)
        t = t->left;
      delete temp;
    }

    return t;
  }

  void inorder(node* t) {
    if(t == NULL)
      return;
    inorder(t->left);
    Rcout << t->data << " ";
    inorder(t->right);
  }

  int height(node* node)
  {
    if (node == NULL)
        return 0;
    else {
        /* compute the height of each subtree */
        int lheight = height(node->left);
        int rheight = height(node->right);
 
        /* use the larger one */
        if (lheight > rheight) {
          return (lheight + 1);
        }
        else {
          return (rheight + 1);
        }
    }
  }

  /* Print nodes at a given level */
  void printGivenLevel(node* root, int level)
  {
    if (root == NULL)
      return;
    if (level == 1)
      Rcout << root->data << " ";
    else if (level > 1) {
      printGivenLevel(root->left,  level - 1);
      printGivenLevel(root->right, level - 1);
    }
  }

  void printLevelOrder(node* root)
  {
    int h = height(root);
    int i;
    for (i = 1; i <= h; i++) {
        printGivenLevel(root, i);
        Rcout << std::endl;
    }
  }

  node* find(node* t, double x) {
    if(t == NULL)
      return NULL;
    else if(x < t->data)
      return find(t->left, x);
    else if(x > t->data)
      return find(t->right, x);
    else
      return t;
  }

public:
  BST() {
    root = NULL;
  }

  ~BST() {
    root = makeEmpty(root);
  }

  void insert(double x, int k) {
    root = insert_(x, k, root);
  }

  void remove(double x) {
    root = remove_(x, root);
  }

  void display() {
    inorder(root);
    Rcout << std::endl;
  }

  void printTree() {
    printLevelOrder(root);
    Rcout << std::endl;
  }

  int search(double x) {
    node *temp = find(root, x);
    if (temp != NULL) {
      return(temp->key);
    } else {
      return(-1);
    }
  }
};

RCPP_MODULE(BinaryTree) {
  class_<BST>("BST")
  .constructor()
  .method("insert",  &BST::insert)
  .method("remove",  &BST::remove)
  .method("display", &BST::display)
  .method("search",  &BST::search)
  .method("printTree",  &BST::printTree)
  ;
}

