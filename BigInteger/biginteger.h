#include <iostream>
#include <vector>
#include <string>
#include <complex>
using std::vector;
using std::complex;
const double pi = 3.14159265358979323846;
class BigInteger {
private:
    vector<int> _digits;
    int _sign;
public:
    static const int base = 100;
    static const int digitLen = 2;
    BigInteger(int);
    BigInteger(): BigInteger(0) {}

    BigInteger& operator+=(const BigInteger& other) {
        if (_sign * other._sign == 1) {
            _digits.resize(std::max(size(), other.size()) + 1);
            for (size_t i = 0; i < other.size(); ++i) {
                _digits[i] += other[i];
            }
            normalize();
        } else {
            _sign = -_sign;
            *this -= other;
            _sign = -_sign;
            nullCheck();
        }
        return *this;
    }
    BigInteger& operator-=(const BigInteger& other) {
        if (_sign * other._sign == 1) {
            _digits.resize(std::max(size(), other.size()));
            for (size_t i = 0; i < other.size(); ++i) {
                _digits[i] -= other[i];
            }
            int leftDigitSign = 1;
            for (int i = static_cast<int>(size()) - 1; i >= 0; --i) {
                if (_digits[i] != 0) {
                    leftDigitSign = _digits[i] < 0 ? -1 : 1;
                    break;
                }
            }
            for (size_t i = 0; i < size(); ++i) {
                _digits[i] *= leftDigitSign;
            }
            _sign *= leftDigitSign;
            normalize();
        } else {
            _sign = -_sign;
            *this += other;
            _sign = -_sign;
            nullCheck();
        }
        return *this;
    }
    BigInteger& operator*=(const BigInteger& other) {
        polyMultiply(_digits, other._digits);
        _sign = _sign * other._sign;
        normalize();
        return *this;
    }
    void shortMultiply(int x) {
        _digits.push_back(0);
        for (size_t i = 0; i < _digits.size(); ++i)
            _digits[i] *= abs(x);
        if(x < 0)
            _sign = -_sign;
        normalize();
    }
    BigInteger& operator/=(const BigInteger& other) {
        *this = division(*this, other);
        return *this;
    }
    BigInteger& operator%=(const BigInteger& other);
    BigInteger operator-() const {
        BigInteger result = *this;
        result._sign = -_sign;
        result.nullCheck();
        return result;
    }
    BigInteger& operator++() {
        if (_sign == 1) {
            size_t i = 0;
            while (_digits[i] + 1 == base) {
                _digits[i] = 0;
                ++i;
                if (i == _digits.size())
                    _digits.push_back(0);
            }
            ++_digits[i];
        } else {
            _sign = 1;
            --*this;
            _sign = -1;
            nullCheck();
        }
        return *this;
    }
    BigInteger& operator--() {
        if (!isNull() && _sign == 1) {
            size_t i = 0;
            while (_digits[i] == 0) {
                _digits[i] = base - 1;
                ++i;
            }
            --_digits[i];
            if (size() > 1 && back() == 0)
                _digits.pop_back();
        } else {
            _sign = 1;
            ++*this;
            _sign = -1;
            nullCheck();
        }
        return *this;
    }
    BigInteger operator++(int) {
        BigInteger result = *this;
        ++*this;
        return result;
    }
    BigInteger operator--(int) {
        BigInteger result = *this;
        --*this;
        return result;
    }
    explicit operator bool() const {
        return !isNull();
    }
    std::string toString() const;
    int sign() const {
        return _sign;
    }
    size_t size() const {
        return _digits.size();
    }
    bool isNull() const {
        return back() == 0;
    }
    void clear() {
        _digits.clear();
        _sign = 1;
    }
    int operator[](int index) const {
        return _digits[index];
    }
    int& operator[](int index) {
        return _digits[index];
    }
    int back() const {
        return _digits.back();
    }
    friend std::istream& operator>>(std::istream&, BigInteger&);
private:
    static BigInteger division(BigInteger dividend, BigInteger divisor);
    BigInteger& basePowMultiply(int pow) {
        if (isNull()) return *this;
        _digits.insert(_digits.begin(), pow, 0);
        return *this;
    }
    static void polyMultiply(vector<int>& poly1, const vector<int>& poly2) {
        vector<complex<double>> dft1(poly1.begin(), poly1.end());
        vector<complex<double>> dft2(poly2.begin(), poly2.end());
        size_t n = 1;
        int logN = 0;
        while (n < poly1.size() + poly2.size()) {
            n *= 2;
            ++logN;
        }
        dft1.resize(n, 0);
        dft2.resize(n, 0);
        fft(dft1, logN);
        fft(dft2, logN);

        for (size_t i = 0; i < n; ++i) {
            dft1[i] *= dft2[i];
        }
        fft(dft1, logN, true);
        poly1.resize(n);
        for (size_t i = 0; i < n; ++i) {
            dft1[i] /= static_cast<double>(n);
            poly1[i] = static_cast<int>(dft1[i].real() + 0.5);
        }
    }
    static int reverseBit(int num, int logN) {
        int result = 0;
        for (int i = 0; i < logN; ++i) {
            if(num & (1 << i))
                result |= (1 << (logN - i - 1));
        }
        return result;
    }
    static void fft(vector<complex<double>>& poly, int logN, bool inverted = false) {
        int n = static_cast<int>(poly.size());
        for (int i = 0; i < n; ++i) {
            if (i < reverseBit(i, logN))
                std::swap(poly[i], poly[reverseBit(i, logN)]);
        }
        for(int len = 2; len <= n; len *= 2) {
            double angle = (inverted ? -1 : 1) * 2 * pi / len;
            complex<double> wn(cos(angle), sin(angle));
            for (int i = 0; i < n; i += len) {
                complex<double> w(1);
                for (int j = 0; j < len / 2; ++j) {
                    complex<double> u = poly[i + j];
                    complex<double> v = w * poly[i + j + len / 2];
                    poly[i + j] = u + v;
                    poly[i + j + len / 2] = u - v;
                    w *= wn;
                }
            }
        }
    }
    void nullCheck() {
        if (back() == 0)
            _sign = 1;
    }
    void normalize() {
        int carry = 0;
        for (size_t i = 0; i < size(); ++i) {
            _digits[i] += carry;
            carry = _digits[i] / base;
            _digits[i] %= base;
            if (_digits[i] < 0) {
                _digits[i] += base;
                --carry;
            }
        }
        while (size() > 1 && _digits.back() == 0) {
            _digits.pop_back();
        }
        nullCheck();
    }
};
BigInteger operator+(const BigInteger& num1, const BigInteger& num2) {
    BigInteger result = num1;
    result += num2;
    return result;
}
BigInteger operator-(const BigInteger& num1, const BigInteger& num2) {
    BigInteger result = num1;
    result -= num2;
    return result;
}
BigInteger operator*(const BigInteger& num1, const BigInteger& num2) {
    BigInteger result = num1;
    result *= num2;
    return result;
}
BigInteger operator/(const BigInteger& num1, const BigInteger& num2) {
    BigInteger result = num1;
    result /= num2;
    return result;
}
BigInteger operator%(const BigInteger& num1, const BigInteger& num2) {
    return num1 - (num1 / num2) * num2;
}
BigInteger& BigInteger::operator%=(const BigInteger& other) {
    *this = *this % other;
    return *this;
}

bool operator==(const BigInteger& num1, const BigInteger& num2) {
    if (num1.sign() != num2.sign() || num1.size() != num2.size()) return false;
    for (int i = static_cast<int>(num1.size()) - 1; i >= 0; --i) {
        if (num1[i] != num2[i]) return false;
    }
    return true;
}
bool operator!=(const BigInteger& num1, const BigInteger& num2) {
    return !(num1 == num2);
}
bool operator<(const BigInteger& num1, const BigInteger& num2) {
    if(num1.size() != num2.size()) return num1.sign() * num1.size() < num2.sign() * num2.size();
    for (int i = static_cast<int>(num1.size()) - 1; i >= 0; --i) {
        if (num1[i] == num2[i]) continue;
        return num1.sign() * num1[i] < num2.sign() * num2[i];
    }
    return false;
}
bool operator>(const BigInteger& num1, const BigInteger& num2) {
     return num2 < num1;
}
bool operator<=(const BigInteger& num1, const BigInteger& num2) {
    return !(num2 < num1);
}
bool operator>=(const BigInteger& num1, const BigInteger& num2) {
    return !(num1 < num2);
}
std::istream& operator>>(std::istream& is, BigInteger& num) {
    num.clear();
    std::string str;
    is >> str;
    if (str[0] == '-') {
        num._sign = -1;
        str.erase(str.begin());
    }
    for (int i = static_cast<int>(str.size()) - 1; i >= 0; i -= BigInteger::digitLen) {
        int index = static_cast<size_t>(std::max(i - BigInteger::digitLen + 1, 0));
        std::string digit = str.substr(index, i - index + 1);
        num._digits.push_back(std::stoi(digit));
    }
    return is;
}
std::ostream& operator<<(std::ostream& os, const BigInteger& num) {
    if (num.sign() == -1)
        os << '-';
    os << num.back();
    for (int i = static_cast<int>(num.size()) - 2; i >= 0; --i) {
        std::string digit = std::to_string(num[i]);
        digit.insert(0, BigInteger::digitLen - static_cast<int>(digit.size()), '0');
        os << digit;
    }
    return os;
}
std::string BigInteger::toString() const {
    std::stringstream ios;
    ios << *this;
    return ios.str();
}
BigInteger::BigInteger(int x) {
    std::stringstream ios;
    ios << x;
    ios >> *this;
}

BigInteger BigInteger::division(BigInteger dividend, BigInteger divisor) {
    int sign1 = dividend.sign();
    int sign2 = divisor.sign();
    dividend._sign = 1;
    divisor._sign = 1;
    int n = static_cast<int>(dividend.size());
    int m = static_cast<int>(divisor.size());
    BigInteger quotient;
    for (int pow = n - m; pow >= 0; --pow) {
        int left = 0;
        int right = base;
        while (left + 1 < right) {
            int mid = (left + right) / 2;
            BigInteger x = divisor;
            x.shortMultiply(mid);
            x.basePowMultiply(pow);
            if (x <= dividend)
                left = mid;
            else
                right = mid;
        }
        BigInteger x = divisor;
        x.shortMultiply(left);
        dividend -= x.basePowMultiply(pow);
        quotient += BigInteger(left).basePowMultiply(pow);
    }
    quotient._sign = sign1 * sign2;
    quotient.nullCheck();
    return quotient;
}
class Rational {
private:
    BigInteger numerator;
    BigInteger denominator = 1;
public:
    Rational(): numerator(0) {}
    Rational(const BigInteger& x, const BigInteger& y = 1): numerator(x), denominator(y) {
        if(denominator.sign() == -1) {
            numerator = -numerator;
            denominator = -denominator;
        }
        normalize();
    }
    Rational(int x): numerator(x) {}
    Rational& operator+=(const Rational& other) {
        numerator *= other.denominator;
        numerator += denominator * other.numerator;
        denominator *= other.denominator;
        normalize();
        return *this;
    }
    Rational& operator-=(const Rational& other) {
        numerator *= other.denominator;
        numerator -= denominator * other.numerator;
        denominator *= other.denominator;
        normalize();
        return *this;
    }
    Rational& operator*=(const Rational& other) {
        numerator *= other.numerator;
        denominator *= other.denominator;
        normalize();
        return *this;
    }
    Rational& operator/=(const Rational& other) {
        numerator *= other.denominator;
        denominator *= other.numerator;
        if(denominator.sign() == -1) {
            numerator = -numerator;
            denominator = -denominator;
        }
        normalize();
        return *this;
    }
    Rational operator-() const {
        return Rational(-numerator, denominator);
    }
    std::string toString() const {
        std::string result = numerator.toString();
        if(denominator != 1)
            result += '/' + denominator.toString();
        return result;
    }
    std::string asDecimal(size_t precision = 0) const {
        BigInteger quotient = numerator;
        for (size_t i = 1; i <= precision; ++i)
            quotient.shortMultiply(10);
        quotient /= denominator;
        int sign = quotient.sign();
        if(sign == -1)
            quotient = -quotient;
        std::string result = quotient.toString();
        if(precision + 1 > result.size())
            result = std::string(precision + 1 - result.size(), '0') + result;
        if(precision > 0)
            result.insert(result.end() - precision, '.');
        if(sign == -1)
            result = '-' + result;
        return result;
    }
    explicit operator double() const {
        std::istringstream is(asDecimal(18));
        double result;
        is >> result;
        return result;
    }
    friend bool operator==(const Rational& num1, const Rational& num2);
    friend bool operator<(const Rational& num1, const Rational& num2);
private:
    void normalize() {
        BigInteger numCopy = numerator;
        BigInteger denCopy = denominator;
        if(numCopy.sign() == -1)
            numCopy = -numCopy;
        BigInteger divisor = gcd(numCopy, denCopy);
        numerator /= divisor;
        denominator /= divisor;
    }
    static BigInteger gcd(BigInteger& a, BigInteger& b) {
        if(b.isNull())
            return a;
        else
            return gcd(b, a %= b);
    }

};
Rational operator+(const Rational& num1, const Rational& num2) {
    Rational result = num1;
    result += num2;
    return result;
}
Rational operator-(const Rational& num1, const Rational& num2) {
    Rational result = num1;
    result -= num2;
    return result;
}
Rational operator*(const Rational& num1, const Rational& num2) {
    Rational result = num1;
    result *= num2;
    return result;
}
Rational operator/(const Rational& num1, const Rational& num2) {
    Rational result = num1;
    result /= num2;
    return result;
}
bool operator==(const Rational& num1, const Rational& num2) {
    return (num1.numerator == num2.numerator) && (num1.denominator == num2.denominator);
}
bool operator!=(const Rational& num1, const Rational& num2) {
    return !(num1 == num2);
}
bool operator<(const Rational& num1, const Rational& num2) {
    return num1.numerator * num2.denominator < num2.numerator * num1.denominator;
}
bool operator>(const Rational& num1, const Rational& num2) {
    return num2 < num1;
}
bool operator<=(const Rational& num1, const Rational& num2) {
    return !(num2 < num1);
}
bool operator>=(const Rational& num1, const Rational& num2) {
    return !(num1 < num2);
}

