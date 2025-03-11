#ifndef UMAP_HXX
#define UMAP_HXX

#include <functional>

#include <assert.h>
#include <stddef.h>

#define UMAP_INIT_CAP 256

template <class K, class V>
struct Umap_Bucket {
        K k;
        V v;
        Umap_Bucket *next;
};

template <class K, class V>
struct Umap {
        struct {
                Umap_Bucket<K, V> **data;
                size_t len;
                size_t cap;
        } _tbl;

        unsigned long (*_hash)(K);
        std::function<bool(K, K)> _compar;

        Umap(std::function<bool(K, K)> compar) {
                _hash = _djb2;
                _init(compar);
        }

        Umap(std::function<bool(K, K)> compar, unsigned long (*hash)(K)) : _hash(hash) {
                _init(compar);
        }

        ~Umap(void) {
                // for (size_t i = 0; i < _tbl.cap; ++i) {
                //         Umap_Bucket<K, V> *current = _tbl.data[i];
                //         while (current) {
                //                 Umap_Bucket<K, V> *next = current->next;
                //                 delete current;
                //                 current = next;
                //         }
                // }
                // delete[] _tbl.data;
        }

        // TODO: check for duplicate
        void add(K k, V v) {
                unsigned long idx = _hash(k) % _tbl.cap;
                auto b = new Umap_Bucket<K, V>;
                b->k = k;
                b->v = v;
                b->next = _tbl.data[idx];
                _tbl.data[idx] = b;
                _tbl.len++;
        }

        V *get(const K &k) {
                unsigned long idx = _hash(k) % _tbl.cap;
                auto b = _tbl.data[idx];
                while (b) {
                        if (_compar(b->k, k)) {
                                return &b->v;
                        }
                        b = b->next;
                }
                return NULL;
        }

        bool has(const K &k) {
                return get(k) != NULL;
        }

private:
        void _init(std::function<bool(K, K)> compar) {
                _compar = compar;
                _tbl.cap = UMAP_INIT_CAP;
                _tbl.len = 0;
                _tbl.data = new Umap_Bucket<K, V>*[_tbl.cap]();
        }

        static unsigned long _djb2(const char *s) {
                unsigned long hash = 5381;
                int c;
                while ((c = *s++)) {
                        hash = ((hash << 5) + hash) + c;
                }
                return hash;
        }

        static unsigned long _djb2(char *s) {
                unsigned long hash = 5381;
                int c;
                while ((c = *s++)) {
                        hash = ((hash << 5) + hash) + c;
                }
                return hash;
        }
};

#endif // UMAP_HXX
