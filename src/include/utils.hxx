#ifndef UTILS_HXX
#define UTILS_HXX

#include <memory>
#include <vector>
#include <optional>

#define ignore(x) (void)(x)

template <typename T> using sh_ptr = std::shared_ptr<T>;
template <typename T> using un_ptr = std::unique_ptr<T>;
template <typename T> using vec = std::vector<T>;
template <typename T> using optional = std::optional<T>;
template <typename T, typename K> using pair = std::pair<T, K>;
template <typename T, typename K> using vec_pair = vec<pair<T, K>>;
using str = std::string;

template <typename T, typename... Args>
std::shared_ptr<T> msh_ptr(Args&&... args);

template <typename T, typename... Args>
std::unique_ptr<T> mun_ptr(Args&&... args);

#endif // UTILS_HXX
