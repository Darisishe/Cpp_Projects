#include <iostream>
#include <cstring>

class String {
private:
    size_t size_ = 0;
    size_t capacity_ = 0;
    char* str_ = nullptr;

public:
    String(const char* str): size_(strlen(str)), capacity_(size_), str_(new char[size_]) {
        strncpy(str_, str, size_);
    }
    String(size_t n, char c): size_(n), capacity_(n), str_(new char[n]) {
        memset(str_, c, n);
    }
    String(char c): String(1, c) {}
    String() {}
    String(const String& s): size_(s.size_), capacity_(s.capacity_), str_(new char[capacity_]) {
        strncpy(str_, s.str_, size_);
    }

    void swap(String& s) {
        std::swap(size_, s.size_);
        std::swap(capacity_, s.capacity_);
        std::swap(str_, s.str_);
    }
    String& operator= (const String& s) {
        String copy = s;
        swap(copy);
        return *this;
    }
    size_t length() const {
        return size_;
    }

    void push_back(char c) {
        if (capacity_ == size_) {
            char* copy = str_;
            capacity_ = 2 * capacity_ + 1;
            str_ = new char[capacity_];
            strncpy(str_, copy, size_);
            delete[] copy;
        }
        str_[size_] = c;
        ++size_;
    }
    void pop_back() {
        --size_;
    }

    char& front() {
        return str_[0];
    }
    char front() const {
        return str_[0];
    }
    char& back() {
        return str_[size_ - 1];
    }
    char back() const {
        return str_[size_ - 1];
    }
    char& operator[](size_t index) {
        return str_[index];
    }
    char operator[](size_t index) const {
        return str_[index];
    }

    String& operator+=(const String& s) {
        for (size_t i = 0; i < s.length(); ++i)
            push_back(s[i]);
        return *this;
    }
    String& operator+=(char c) {
        push_back(c);
        return *this;
    }

    size_t find(const String& substr) const {
        for (size_t i = 0; i < length(); ++i) {
            if (i + substr.length() > length()) break;
            for (size_t j = 0; j < substr.length(); ++j) {
                if ((*this)[i + j] != substr[j])
                    break;
                if (j == substr.length() - 1)
                    return i;
            }
        }
        return length();
    }
    String reversed() const {
        String result;
        for (int i = static_cast<int>(length()) - 1; i >= 0; --i)
            result += (*this)[i];
        return result;
    }
    size_t rfind(const String& substr) const {
        size_t index = reversed().find(substr.reversed());
        if (index == length())
            return length();
        return length() - index - substr.length();
    }
    String substr(size_t start, size_t count) const {
        String result;
        for (size_t i = start; i < start + count; ++i)
            result += (*this)[i];
        return result;
    }

    bool empty() const {
        return size_ == 0;
    }
    void clear() {
        size_ = 0;
        capacity_ = 0;
        delete[] str_;
        str_ = nullptr;
    }
    ~String() {
        delete[] str_;
    }
};

String operator+(const String& str1, const String& str2)  {
    String result = str1;
    return result += str2;
}

std::ostream& operator<<(std::ostream& os, const String& s) {
    for (size_t i = 0; i < s.length(); ++i) {
        os << s[i];
    }
    return os;
}
std::istream& operator>>(std::istream& is, String& s) {
    s.clear();
    char c;
    while (is.get(c) && !isspace(c)) {
        s += c;
    }
    return is;
}

bool operator==(const String& s1, const String& s2) {
    if (s1.length() != s2.length())
        return false;
    for (size_t i = 0; i < s1.length(); ++i)
    {
        if (s1[i] != s2[i])
            return false;
    }
    return true;
}