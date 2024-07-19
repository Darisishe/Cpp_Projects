#include <iostream>
#include <memory>


template <size_t N>
class StackStorage {
private:
    char data[N];
    char* pool;
    size_t space;
public:
    StackStorage() : pool(data), space(N) {}
    StackStorage(const StackStorage<N>&) = delete;
    StackStorage& operator=(const StackStorage<N>&) = delete;

    void* allocate(size_t alignment, size_t size) {
        void* result = pool;
        std::align(alignment, size, result, space);
        pool = reinterpret_cast<char*>(result) + size;
        space -= size;
        return result;
    }
};


template <typename T, size_t N>
class StackAllocator {
private:
    StackStorage<N>* storage;
public:
    using value_type = T;
    StackAllocator() = delete;
    StackAllocator(StackStorage<N>& st) : storage(&st) {}

    template<typename U>
    StackAllocator(const StackAllocator<U, N>& other) : storage(other.getStorage()) {}

    StackStorage<N>* getStorage() const {
        return storage;
    }

    ~StackAllocator() = default;

    T* allocate(size_t n) {
        void* ptr = storage->allocate(alignof(T), n * sizeof(T));
        return reinterpret_cast<T*>(ptr);
    }

    void deallocate(T*, size_t) {}

    template <typename... Args>
    void construct(T* ptr, const Args&... args) {
        new(ptr) T(args...);
    }

    void destroy(T* ptr) {
        ptr->~T();
    }

    StackAllocator<T, N> select_on_container_copy_construction() const {
        return *this;
    }

    template<typename U>
    struct rebind {
        using other = StackAllocator<U, N>;
    };
};



template <typename T, typename Allocator = std::allocator<T>>
class List {
private:
    struct BaseNode {
        BaseNode* prev;
        BaseNode* next;

        BaseNode() : prev(this), next(this) {}

        BaseNode(BaseNode* prev, BaseNode* next)
            : prev(prev), next(next) {}
    };

    struct Node : BaseNode {
        T value;

        template <typename... Args>
        Node(BaseNode* prev, BaseNode* next, Args&&... args)
            : BaseNode(prev, next), value(std::forward<Args>(args)...) {}
    };

    using NodeAllocator = typename std::allocator_traits<Allocator>::template rebind_alloc<Node>;
    using AllocTraits = std::allocator_traits<NodeAllocator>;

    NodeAllocator nodeAlloc;
    mutable BaseNode fakeNode; //mutable, т.к. иначе &fakeNode может давать const указатель, и будут проблемы с конструктором const_iterator
    size_t sz = 0;

public:
    template <bool IsConst>
    class common_iterator {
    private:
        BaseNode* ptr;
    public:
        friend List;
        using difference_type = std::ptrdiff_t;
        using value_type = std::conditional_t<IsConst, const T, T>;
        using pointer = std::conditional_t<IsConst, const T*, T*>;
        using reference = std::conditional_t<IsConst, const T&, T&>;
        using iterator_category = std::bidirectional_iterator_tag;

        common_iterator(BaseNode* ptr) : ptr(ptr) {}
        common_iterator(const common_iterator<false>& it) : ptr(it.ptr) {}

        common_iterator& operator=(const common_iterator<false>& it) {
            ptr = it.ptr;
            return *this;
        }

        reference operator*() {
            return static_cast<Node*>(ptr)->value;
        }

        pointer operator->() {
            return &(static_cast<Node*>(ptr)->value);
        }

        common_iterator& operator++() {
            ptr = ptr->next;
            return *this;
        }

        common_iterator operator++(int) {
            common_iterator result = *this;
            ptr = ptr->next;
            return result;
        }

        common_iterator& operator--() {
            ptr = ptr->prev;
            return *this;
        }

        common_iterator operator--(int) {
            common_iterator result = *this;
            ptr = ptr->prev;
            return result;
        }

        bool operator==(const common_iterator& it) const {
            return ptr == it.ptr;
        }

        bool operator!=(const common_iterator& it) const {
            return ptr != it.ptr;
        }
    };

    using iterator = common_iterator<false>;
    using const_iterator = common_iterator<true>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    iterator begin() {
        return iterator(fakeNode.next);
    }

    iterator end() {
        return iterator(&fakeNode);
    }

    const_iterator begin() const {
        return const_iterator(fakeNode.next);
    }

    const_iterator end() const {
        return const_iterator(&fakeNode);
    }

    const_iterator cbegin() const {
        return const_iterator(fakeNode.next);
    }

    const_iterator cend() const {
        return const_iterator(&fakeNode);
    }

    reverse_iterator rbegin() const {
        return reverse_iterator(&fakeNode);
    }

    reverse_iterator rend() const {
        return reverse_iterator(fakeNode.next);
    }

    const_reverse_iterator crbegin() const {
        return const_reverse_iterator(&fakeNode);
    }

    const_reverse_iterator crend() const {
        return const_reverse_iterator(fakeNode.next);
    }

    List() {}

    List(size_t count, const T& value, const Allocator& alloc = Allocator()) : nodeAlloc(alloc) {
        insert(end(), count, value);
    }

    List(const Allocator& alloc) : nodeAlloc(alloc) {}

    List(size_t count, const Allocator& alloc = Allocator()) : nodeAlloc(alloc) {
        for (size_t i = 0; i < count; ++i) {
            try {
                emplace(end());
            } catch (...) {
                erase(begin(), end());
                throw;
            }
        }
    }

    List(const List& other) 
        : List(other, AllocTraits::select_on_container_copy_construction(other.nodeAlloc)) {}

    void swap(List& other) { 
        std::swap(sz, other.sz);
        std::swap(nodeAlloc, other.nodeAlloc);
        std::swap(fakeNode, other.fakeNode);
        this->restorePointers();
        other.restorePointers();
    }

    List& operator=(const List& other) {
        List temporary(other, AllocTraits::propagate_on_container_copy_assignment::value ? other.nodeAlloc : nodeAlloc);
        swap(temporary);
        return *this;
    }

    ~List() { 
        erase(begin(), end());
    }

    Allocator get_allocator() const {
        return Allocator(nodeAlloc);
    }

    size_t size() const {
        return sz;
    }

    void push_back(const T& value) {
        insert(end(), value);
    }

    void push_front(const T& value) {
        insert(begin(), value);
    }

    void pop_back() {
        erase(--end());
    }

    void pop_front() {
        erase(begin());
    }

    iterator insert(const_iterator pos, const T& value) {
        return emplace(pos, value);
    }

    iterator insert(const_iterator pos, size_t count, const T& value) {
        iterator it(pos.ptr);
        try {
            for (size_t i = 0; i < count; ++i)
                it = insert(it, value);
        } catch (...) {
            erase(it, pos);
            throw;
        }
        return it;
    }

    template <typename... Args>
    iterator emplace(const_iterator pos, Args&&... args) {
        Node* ptr = AllocTraits::allocate(nodeAlloc, 1);
        try {
            AllocTraits::construct(nodeAlloc, ptr, pos.ptr->prev, pos.ptr, std::forward<Args>(args)...);
        } catch (...) {
            AllocTraits::deallocate(nodeAlloc, ptr, 1);
            throw;
        }
        pos.ptr->prev->next = static_cast<BaseNode*>(ptr);
        pos.ptr->prev = static_cast<BaseNode*>(ptr);
        ++sz;
        return iterator(ptr);
    }

    void erase(const_iterator pos) {
        pos.ptr->prev->next = pos.ptr->next;
        pos.ptr->next->prev = pos.ptr->prev;

        AllocTraits::destroy(nodeAlloc, static_cast<Node*>(pos.ptr));
        AllocTraits::deallocate(nodeAlloc, static_cast<Node*>(pos.ptr), 1);
        --sz;
    }

    void erase(const_iterator it, const_iterator end) {
        while (it != end) {
            erase(it++);
        }
    }

private:
    List(const List& other, const NodeAllocator& nodeAlloc) : nodeAlloc(nodeAlloc) {
        try {
            for (const_iterator it = other.cbegin(); it != other.cend(); ++it) {
                insert(end(), *it);
            }
        } catch (...) {
            erase(begin(), end());
            throw;
        }
    }

    void restorePointers() {
        fakeNode.next->prev = &fakeNode;
        fakeNode.prev->next = &fakeNode;
    }
};
