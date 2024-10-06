#ifndef UTILS_HXX
#define UTILS_HXX

#include <memory>
#include <vector>

template <typename T> using sh_ptr = std::shared_ptr<T>;
template <typename T> using un_ptr = std::unique_ptr<T>;
template <typename T> using vec = std::vector<T>;
using str = std::string;

template <typename T, typename... Args>
std::shared_ptr<T> msh_ptr(Args&&... args);

template <typename T, typename... Args>
std::shared_ptr<T> mun_ptr(Args&&... args);

#endif // UTILS_HXX
