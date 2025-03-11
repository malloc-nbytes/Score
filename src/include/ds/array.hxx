#ifndef ARRAY_HXX
#define ARRAY_HXX

#include <utility>

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

#include "err.hxx"

template <class T>
struct Array {
        T *_data;
        size_t _len, _cap;

        Array(void) : _data(NULL), _len(0), _cap(0) {}

        Array(const Array &other) : _data(nullptr), _len(other._len), _cap(other._cap) {
                if (_cap > 0) {
                        _data = new T[_cap];
                        for (size_t i = 0; i < _len; ++i) {
                                _data[i] = other._data[i];
                        }
                }
        }

        ~Array(void) {
                delete[] _data;
        }

        void add(T d) {
                if (_len >= _cap) {
                        _resize();
                }
                _data[_len++] = d;
        }

        bool contains(T &d) const {
                for (size_t i = 0; i < _len; ++i) {
                        if (_data[i] == d) {
                                return true;
                        }
                }
                return false;
        }

        size_t length(void) const {
                return _len;
        }

        T &back(void) {
                return _data[_len-1];
        }

        const T &back(void) const {
                return _data[_len-1];
        }

        void pop_back(void) {
                --_len;
        }

        Array &operator=(const Array &other) {
                if (this == &other) {
                        return *this;
                }

                // Free existing resources
                delete[] _data;

                // Copy size and capacity
                _len = other._len;
                _cap = other._cap;

                // Allocate new memory and copy elements
                if (_cap > 0) {
                        _data = new T[_cap];
                        for (size_t i = 0; i < _len; ++i) {
                                _data[i] = other._data[i];
                        }
                } else {
                        _data = nullptr;
                }

                return *this;
        }


        T &operator[](size_t i) {
                _assert_inbounds(i);
                return _data[i];
        }

        const T &operator[](size_t i) const {
                _assert_inbounds(i);
                return _data[i];
        }

private:
        void _resize(void) {
                size_t ocap = _cap;
                _cap = _cap ? _cap * 2 : 2;

                void* raw_memory = operator new(_cap * sizeof(T));
                T* dx = static_cast<T*>(raw_memory);

                for (size_t i = 0; i < ocap; ++i) {
                        new (&dx[i]) T(std::move(_data[i]));
                        _data[i].~T();
                }

                if (ocap != 0) {
                        delete[] _data;
                }

                _data = dx;
        }

        void _assert_inbounds(size_t i) {
                if (i >= _len) {
                        err_wargs("index %zu is out of bounds of length %zu",
                                  i, _len);
                }
        }
};

#endif // ARRAY_HXX
