#ifndef SCOPE_HXX
#define SCOPE_HXX

#include <iostream>
#include <memory>
#include <vector>
#include <map>

#include "utils.hxx"

namespace scope {
    template <typename K, typename V>
    struct t {
        vec<map<K, V>> values;
    };

    template <typename K, typename V>
    inline void add(scope::t<K, V> &s, K k, V &v) {
        s.values.emplace(k, v);
    }

    template <typename K, typename V>
    inline bool contains(scope::t<K, V> &s, const K &k) {
        for (const auto &m : s.values)
            if (m.find(k) != m.end())
                return true;
        return false;
    }

    template <typename K, typename V>
    inline V &get(scope::t<K, V> &s, const K &k) {
        for (auto it = s.values.rbegin(); it != s.values.rend(); ++it) {
            if ((*it).find(k) != (*it).end())
                return (*it).at(k);
        }
        std::cerr << "scope get: tried to get value that doesn't exist" << std::endl;
        std::exit(1);
    }

    template <typename K, typename V>
    inline void push(scope::t<K, V> &s) {
        s.values.emplace_back();
    }

    template <typename K, typename V>
    inline void pop(scope::t<K, V> &s) {
        if (s.size() <= 0) {
            std::cerr << "tried to pop scope when the size is 0" << std::endl;
            std::exit(1);
        }
        s.values.pop_back();
    }

};

#endif // SCOPE_HXX
