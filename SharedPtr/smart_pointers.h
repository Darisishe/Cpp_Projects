#include <memory>
#include <type_traits>
#include <stdexcept>
struct BaseControlBlock {
    size_t sharedCount = 0;
    size_t weakCount = 0;
    virtual ~BaseControlBlock() noexcept = default;
    virtual void destroyObject() = 0;
    virtual void destroyDeallocate() noexcept = 0;
    virtual void* getPtr() noexcept = 0;
};

template <typename U, typename Deleter, typename Alloc>
struct ControlBlockDirect: BaseControlBlock {
    U* ptr;
    Deleter deleter;
    Alloc alloc;

    ControlBlockDirect(U* ptr, const Deleter& deleter, const Alloc& alloc)
            : ptr(ptr), deleter(deleter), alloc(alloc) {}

    void destroyObject() override {
        deleter(ptr);
    }

    void destroyDeallocate() noexcept override {
        using AllocBlock = typename std::allocator_traits<Alloc>::
        template rebind_alloc<ControlBlockDirect<U, Deleter, Alloc>>;
        AllocBlock allocBlock = alloc;
        alloc.~Alloc();
        deleter.~Deleter();
        std::allocator_traits<AllocBlock>::deallocate(allocBlock, this, 1);
    }

    void* getPtr() noexcept override {
        return ptr;
    }

};

template <typename T, typename Alloc>
struct ControlBlockAlloc: BaseControlBlock {
    Alloc alloc;
    T object;

    template <typename... Args>
    ControlBlockAlloc(const Alloc& alloc, Args&&... args)
            : alloc(alloc), object(std::forward<Args>(args)...) {}

    void destroyObject() noexcept override {
        std::allocator_traits<Alloc>::destroy(alloc, &object);
    }

    void destroyDeallocate() noexcept override {
        using AllocBlock = typename std::allocator_traits<Alloc>::
        template rebind_alloc<ControlBlockAlloc<T, Alloc>>;
        AllocBlock allocBlock = alloc;
        alloc.~Alloc();
        std::allocator_traits<AllocBlock>::deallocate(allocBlock, this, 1);
    }

    void* getPtr() noexcept override {
        return &object;
    }
};

template <typename T>
class SharedPtr {

    template <typename U, typename Alloc, typename... Args>
    friend SharedPtr<U> allocateShared(const Alloc& alloc, Args&&... args);

    template <typename U, typename... Args>
    friend SharedPtr<U> makeShared(Args&&... args);

    template <typename U>
    friend class SharedPtr;

    template <typename U>
    friend class WeakPtr;

private:
    T* ptr;
    BaseControlBlock* block;

public:
    SharedPtr() noexcept: ptr(nullptr), block(nullptr) {}

    template <typename U, typename Deleter = std::default_delete<U>, typename Alloc = std::allocator<U>>
    SharedPtr<T>(U* p, const Deleter& deleter = Deleter(), const Alloc& alloc = Alloc()) {
        using AllocBlock = typename std::allocator_traits<Alloc>::
        template rebind_alloc<ControlBlockDirect<U, Deleter, Alloc>>;
        AllocBlock allocBlock = alloc;
        using AllocBlockTraits = std::allocator_traits<AllocBlock>;
        auto directBlock = AllocBlockTraits::allocate(allocBlock, 1);
        new(directBlock) ControlBlockDirect<U, Deleter, Alloc> (p, deleter, alloc);
        directBlock->sharedCount = 1;
        block = static_cast<BaseControlBlock*>(directBlock);
        ptr = static_cast<T*>(directBlock->ptr);
    }

    template <typename U>
    SharedPtr(const SharedPtr<U>& other) noexcept
            : ptr(static_cast<T*>(other.ptr)), block(other.block) {
        if (block)
            ++block->sharedCount;
    }

    SharedPtr(const SharedPtr& other) noexcept
            : ptr(other.ptr), block(other.block) {
        if (block)
            ++block->sharedCount;
    }

    template <typename U>
    SharedPtr<T>(SharedPtr<U>&& other) noexcept
            : ptr(static_cast<T*>(other.ptr)), block(other.block) {
        other.ptr = nullptr;
        other.block = nullptr;
    }

    template <typename U>
    SharedPtr<T>& operator=(const SharedPtr<U>& other) noexcept {
        SharedPtr<T> temporary(other);
        swap(temporary);
        return *this;
    }

    SharedPtr<T>& operator=(const SharedPtr& other) noexcept {
        SharedPtr<T> temporary(other);
        swap(temporary);
        return *this;
    }

    template <typename U>
    SharedPtr<T>& operator=(SharedPtr<U>&& other) noexcept {
        SharedPtr<T> temporary(std::move(other));
        swap(temporary);
        return *this;
    }

    size_t use_count() const noexcept {
        if (block)
            return block->sharedCount;
        return 0;
    }

    template <typename U>
    void reset(U* ptr) {
        SharedPtr<T> temporary(ptr);
        swap(temporary);
    }

    void reset() {
        SharedPtr<T> temporary;
        swap(temporary);
    }

    ~SharedPtr() noexcept {
        if (block == nullptr) return;
        --block->sharedCount;
        if (block->sharedCount == 0) {
            block->destroyObject();
            if (block->weakCount == 0) {
                block->destroyDeallocate();
            }
        }
    }

    T& operator*() const noexcept {
        return *ptr;
    }

    T* operator->() const noexcept {
        return ptr;
    }

    T* get() const noexcept {
        return ptr;
    }

    void swap(SharedPtr<T>& other) noexcept {
        std::swap(ptr, other.ptr);
        std::swap(block, other.block);
    }

private:
    SharedPtr(T* ptr, BaseControlBlock* block) noexcept
            : ptr(ptr), block(block) {}
};

template <typename T, typename Alloc, typename... Args>
SharedPtr<T> allocateShared(const Alloc& alloc, Args&&... args) {
    using AllocBlock = typename std::allocator_traits<Alloc>::
    template rebind_alloc<ControlBlockAlloc<T, Alloc>>;
    AllocBlock allocBlock = alloc;
    auto block = std::allocator_traits<AllocBlock>::allocate(allocBlock, 1);
    std::allocator_traits<AllocBlock>::construct(allocBlock, block, alloc, std::forward<Args>(args)...);
    block->sharedCount = 1;
    return SharedPtr<T>(&(block->object), static_cast<BaseControlBlock*>(block));
}

template <typename T, typename... Args>
SharedPtr<T> makeShared(Args&&... args) {
    return allocateShared<T>(std::allocator<T>(), std::forward<Args>(args)...);
}

template <typename T>

class WeakPtr {

    template <typename U>
    friend class WeakPtr;

private:
    BaseControlBlock* block;

public:
    WeakPtr() noexcept: block(nullptr) {}

    template <typename U>
    WeakPtr<T>(const SharedPtr<U>& other) noexcept
            : block(other.block) {
        static_assert(std::is_base_of_v<T, U> || std::is_same_v<T, U>);
        if (block)
            ++block->weakCount;
    }

    template <typename U>
    WeakPtr<T>(const WeakPtr<U>& other) noexcept
            : block(other.block) {
        static_assert(std::is_base_of_v<T, U> || std::is_same_v<T, U>);
        if (block)
            ++block->weakCount;
    }

    WeakPtr(const WeakPtr& other) noexcept
            : block(other.block) {
        if (block)
            ++block->weakCount;
    }

    template <typename U>
    WeakPtr<T>(WeakPtr<U>&& other) noexcept
            : block(other.block) {
        static_assert(std::is_base_of_v<T, U> || std::is_same_v<T, U>);
        other.block = nullptr;
    }

    template <typename U>
    WeakPtr<T>& operator=(const WeakPtr<U>& other) noexcept {
        WeakPtr<T> temporary(other);
        swap(temporary);
        return *this;
    }

    WeakPtr<T>& operator=(const WeakPtr& other) noexcept {
        WeakPtr<T> temporary(other);
        swap(temporary);
        return *this;
    }

    template <typename U>
    WeakPtr<T>& operator=(WeakPtr<U>&& other) noexcept {
        WeakPtr<T> temporary(std::move(other));
        swap(temporary);
        return *this;
    }

    void swap(WeakPtr<T>& other) noexcept {
        std::swap(block, other.block);
    }

    bool expired() const noexcept {
        return block->sharedCount == 0;
    }

    SharedPtr<T> lock() const noexcept {
        if (expired())
            return SharedPtr<T>();
        ++block->sharedCount;
        return SharedPtr<T>(static_cast<T*>(block->getPtr()), block);
    }

    size_t use_count() const noexcept {
        if (block == nullptr)
            return 0;
        return block->sharedCount;
    }

    ~WeakPtr() noexcept {
        if (block == nullptr) return;
        --block->weakCount;
        if (expired() && (block->weakCount == 0))
            block->destroyDeallocate();
    }

    T& operator*() const noexcept {
        if (expired())
            throw std::logic_error("Object is dead");
        return *static_cast<T*>(block->getPtr());
    }

    T* operator->() const noexcept {
        if (expired())
            throw std::logic_error("Object is dead");
        return static_cast<T*>(block->getPtr());
    }
};
