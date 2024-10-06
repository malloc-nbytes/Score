#include "utils.hxx"

template <typename T, typename... Args>
std::shared_ptr<T> msh_ptr(Args&&... args) {
    return std::make_shared<T>(std::forward<Args>(args)...);
}

template <typename T, typename... Args>
std::unique_ptr<T> mun_ptr(Args&&... args) {
    return std::make_unique<T>(std::forward<Args>(args)...);
}
