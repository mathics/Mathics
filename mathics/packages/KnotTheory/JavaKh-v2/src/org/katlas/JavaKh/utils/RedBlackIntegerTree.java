package org.katlas.JavaKh.utils;

import java.util.Iterator;

import net.tqft.iterables.AbstractIterator;

/* Copyright (c) 2008 the authors listed at the following URL, and/or
the authors of referenced articles or incorporated external code:
http://en.literateprograms.org/Red-black_tree_(Java)?action=history&offset=20080130152141

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Retrieved from: http://en.literateprograms.org/Red-black_tree_(Java)?oldid=12375
*/

enum Color { RED, BLACK }

class Node<V>
{
    public int key;
    public V value;
    public Node<V> left;
    public Node<V> right;
    public Node<V> parent;
    public Color color;

    public Node(int key, V value, Color nodeColor, Node<V> left, Node<V> right) {
        this.key = key;
        this.value = value;
        this.color = nodeColor;
        this.left = left;
        this.right = right;
        if (left  != null)  left.parent = this;
        if (right != null) right.parent = this;
        this.parent = null;
    }

    public Node<V> grandparent() {
        assert parent != null; // Not the root node
        assert parent.parent != null; // Not child of root
        return parent.parent;
    }

    public Node<V> sibling() {
        assert parent != null; // Root node has no sibling
        if (this == parent.left)
            return parent.right;
        else
            return parent.left;
    }

    public Node<V> uncle() {
        assert parent != null; // Root node has no uncle
        assert parent.parent != null; // Children of root have no uncle
        return parent.sibling();
    }

    public Node<V> greatestDescendent() {
		Node<V> n = this;
		while (n.right != null) {
			n = n.right;
		}
		return n;
	}
    
    public Node<V> leastDescendent() {
		Node<V> n = this;
		while (n.left != null) {
			n = n.left;
		}
		return n;
	}
    
    public Node<V> predecessor() {
    	if(left == null) {
    		if(parent == null) return null;
    	
    		Node<V> n = this;
    		while(n == n.parent.left) {
    			n = n.parent;
    			if(n.parent == null) {
    				return null;
    			}
    		}
    		return n.parent;
    	} else {
    		return left.greatestDescendent();
    	}
    }
    
    public Node<V> successor() {
    	if(right == null) {
    		if(parent == null) return null;
    	
    		Node<V> n = this;
    		while(n == n.parent.right) {
    			n = n.parent;
    			if(n.parent == null) {
    				return null;
    			}
    		}
    		return n.parent;
    	} else {
    		return right.leastDescendent();
    	}
    }
   
}

public class RedBlackIntegerTree<V>
{
	
	private int size;
	public Node<V> root;

    public RedBlackIntegerTree() {
    	size = 0;
        root = null;
    }

 
    private static Color nodeColor(Node<?> n) {
        return n == null ? Color.BLACK : n.color;
    }

    public void clear() {
    	size = 0;
    	root = null;
    }
    
    public Iterable<Integer> keys() {
    	return new Iterable<Integer>() {

			public Iterator<Integer> iterator() {
				return new AbstractIterator<Integer>() {

					Node<V> next = (root == null) ? null : root.leastDescendent();
					
					@Override
					public boolean hasNext() {
						return next != null;
					}

					@Override
					protected Integer returnNext() {
						int r = next.key;
						next = next.successor();
						return r;
					}
			
				};
			} 
    		
    	};
    	
//    	if(null == root) {
//    		return Iterables.emptyIterable();
//    	} else {
//    	return Iterables.transform(root.descendents(), new Transformer<Node<V>, Integer>() {
//			public Integer evaluate(Node<V> s) {
//				return s.key;
//			}
//    		});
//    	}
    }
    
    private Node<V> getNode(int key) {
        Node<V> n = root;
        while (n != null) {
            int compResult = key - n.key;
            if (compResult == 0) {
                return n;
            } else if (compResult < 0) {
                n = n.left;
            } else {
                assert compResult > 0;
                n = n.right;
            }
        }
        return n;
    }

    public boolean containsKey(int key) {
    	return null != getNode(key);
    }
    
    public V get(int key) {
        Node<V> n = getNode(key);
        return n == null ? null : n.value;
    }

    public void decrementIndexesAbove(int key) {
    	if(root == null) return;
		Node<V> node = root.greatestDescendent();
		while(node != null && node.key > key) {
			--node.key;
			node = node.predecessor();
		}
	}
    
    private void rotateLeft(Node<V> n) {
        Node<V> r = n.right;
        replaceNode(n, r);
        n.right = r.left;
        if (r.left != null) {
            r.left.parent = n;
        }
        r.left = n;
        n.parent = r;
    }

    private void rotateRight(Node<V> n) {
        Node<V> l = n.left;
        replaceNode(n, l);
        n.left = l.right;
        if (l.right != null) {
            l.right.parent = n;
        }
        l.right = n;
        n.parent = l;
    }

    private void replaceNode(Node<V> oldn, Node<V> newn) {
        if (oldn.parent == null) {
            root = newn;
        } else {
            if (oldn == oldn.parent.left)
                oldn.parent.left = newn;
            else
                oldn.parent.right = newn;
        }
        if (newn != null) {
            newn.parent = oldn.parent;
        }
    }

    public void put(int key, V value) {
        Node<V> insertedNode = new Node<V>(key, value, Color.RED, null, null);
        if (root == null) {
            root = insertedNode;
        } else {
            Node<V> n = root;
            while (true) {
                int compResult = key - n.key;
                if (compResult == 0) {
                    n.value = value;
                    return;
                } else if (compResult < 0) {
                    if (n.left == null) {
                        n.left = insertedNode;
                        break;
                    } else {
                        n = n.left;
                    }
                } else {
                    assert compResult > 0;
                    if (n.right == null) {
                        n.right = insertedNode;
                        break;
                    } else {
                        n = n.right;
                    }
                }
            }
            insertedNode.parent = n;
        }
        ++size;
        insertCase1(insertedNode);
    }

    private void insertCase1(Node<V> n) {
        if (n.parent == null)
            n.color = Color.BLACK;
        else
            insertCase2(n);
    }

    private void insertCase2(Node<V> n) {
        if (nodeColor(n.parent) == Color.BLACK)
            return; // Tree is still valid
        else
            insertCase3(n);
    }

    void insertCase3(Node<V> n) {
        if (nodeColor(n.uncle()) == Color.RED) {
            n.parent.color = Color.BLACK;
            n.uncle().color = Color.BLACK;
            n.grandparent().color = Color.RED;
            insertCase1(n.grandparent());
        } else {
            insertCase4(n);
        }
    }

    void insertCase4(Node<V> n) {
        if (n == n.parent.right && n.parent == n.grandparent().left) {
            rotateLeft(n.parent);
            n = n.left;
        } else if (n == n.parent.left && n.parent == n.grandparent().right) {
            rotateRight(n.parent);
            n = n.right;
        }
        insertCase5(n);
    }

    void insertCase5(Node<V> n) {
        n.parent.color = Color.BLACK;
        n.grandparent().color = Color.RED;
        if (n == n.parent.left && n.parent == n.grandparent().left) {
            rotateRight(n.grandparent());
        } else {
            assert n == n.parent.right && n.parent == n.grandparent().right;
            rotateLeft(n.grandparent());
        }
    }

    public void remove(int key) {
        Node<V> n = getNode(key);
        if (n == null) {
            return;  // Key not found, do nothing
        } else {
        	--size;
        }
        if (n.left != null && n.right != null) {
            // Copy key/value from predecessor and then delete it instead
            Node<V> pred = n.left.greatestDescendent();
            n.key   = pred.key;
            n.value = pred.value;
            n = pred;
        }

        assert n.left == null || n.right == null;
        Node<V> child = (n.right == null) ? n.left : n.right;
        if (nodeColor(n) == Color.BLACK) {
            n.color = nodeColor(child);
            deleteCase1(n);
        }
        replaceNode(n, child);

    }

    private void deleteCase1(Node<V> n) {
        if (n.parent == null)
            return;
        else
            deleteCase2(n);
    }

    private void deleteCase2(Node<V> n) {
        if (nodeColor(n.sibling()) == Color.RED) {
            n.parent.color = Color.RED;
            n.sibling().color = Color.BLACK;
            if (n == n.parent.left)
                rotateLeft(n.parent);
            else
                rotateRight(n.parent);
        }
        deleteCase3(n);
    }

    private void deleteCase3(Node<V> n) {
        if (nodeColor(n.parent) == Color.BLACK &&
            nodeColor(n.sibling()) == Color.BLACK &&
            nodeColor(n.sibling().left) == Color.BLACK &&
            nodeColor(n.sibling().right) == Color.BLACK)
        {
            n.sibling().color = Color.RED;
            deleteCase1(n.parent);
        }
        else
            deleteCase4(n);
    }

    private void deleteCase4(Node<V> n) {
        if (nodeColor(n.parent) == Color.RED &&
            nodeColor(n.sibling()) == Color.BLACK &&
            nodeColor(n.sibling().left) == Color.BLACK &&
            nodeColor(n.sibling().right) == Color.BLACK)
        {
            n.sibling().color = Color.RED;
            n.parent.color = Color.BLACK;
        }
        else
            deleteCase5(n);
    }

    private void deleteCase5(Node<V> n) {
        if (n == n.parent.left &&
            nodeColor(n.sibling()) == Color.BLACK &&
            nodeColor(n.sibling().left) == Color.RED &&
            nodeColor(n.sibling().right) == Color.BLACK)
        {
            n.sibling().color = Color.RED;
            n.sibling().left.color = Color.BLACK;
            rotateRight(n.sibling());
        }
        else if (n == n.parent.right &&
                 nodeColor(n.sibling()) == Color.BLACK &&
                 nodeColor(n.sibling().right) == Color.RED &&
                 nodeColor(n.sibling().left) == Color.BLACK)
        {
            n.sibling().color = Color.RED;
            n.sibling().right.color = Color.BLACK;
            rotateLeft(n.sibling());
        }
        deleteCase6(n);
    }

    private void deleteCase6(Node<V> n) {
        n.sibling().color = nodeColor(n.parent);
        n.parent.color = Color.BLACK;
        if (n == n.parent.left) {
            assert nodeColor(n.sibling().right) == Color.RED;
            n.sibling().right.color = Color.BLACK;
            rotateLeft(n.parent);
        }
        else
        {
            assert nodeColor(n.sibling().left) == Color.RED;
            n.sibling().left.color = Color.BLACK;
            rotateRight(n.parent);
        }
    }


	public int size() {
		return size;
	}

}

