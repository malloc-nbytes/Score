#include "utils.hxx"

template <typename T, typename... Args>
std::shared_ptr<T> mshptr(Args&&... args) {
    return std::make_shared<T>(std::forward<Args>(args)...);
}

template <typename T, typename... Args>
std::unique_ptr<T> munptr(Args&&... args) {
    return std::make_shared<T>(std::forward<Args>(args)...);
}
