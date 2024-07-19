#include <iostream>
#include <vector>
#include <string>
#include <complex>
#include <initializer_list>
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
        if (x < 0)
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
    bool isEven() {
        return _digits[0] % 2 == 0;
    }
    void divideByTwo() { //разделить четное положительное на 2
        for (int i = static_cast<int>(size()) - 1; i >= 0; --i) {
            if (_digits[i] % 2) {
                --_digits[i];
                _digits[i - 1] += base;
            }
            _digits[i] /= 2;
        }
        normalize();
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
            if (num & (1 << i))
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
        for (int len = 2; len <= n; len *= 2) {
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
    if (num1.size() != num2.size()) return num1.sign() * num1.size() < num2.sign() * num2.size();
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
        if (denominator.sign() == -1) {
            numerator = -numerator;
            denominator = -denominator;
        }
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
        if (denominator.sign() == -1) {
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
        if (denominator != 1)
            result += '/' + denominator.toString();
        return result;
    }
    std::string asDecimal(size_t precision = 0) const {
        BigInteger quotient = numerator;
        for (size_t i = 1; i <= precision; ++i)
            quotient.shortMultiply(10);
        quotient /= denominator;
        int sign = quotient.sign();
        if (sign == -1)
            quotient = -quotient;
        std::string result = quotient.toString();
        if (precision + 1 > result.size())
            result = std::string(precision + 1 - result.size(), '0') + result;
        if (precision > 0)
            result.insert(result.end() - precision, '.');
        if (sign == -1)
            result = '-' + result;
        return result;
    }
    explicit operator double() const {
        std::istringstream is(asDecimal(18));
        double result;
        is >> result;
        return result;
    }
    friend bool operator==(const Rational&, const Rational& );
    friend bool operator<(const Rational&, const Rational& );
    friend std::istream& operator>>(std::istream&, Rational& );
private:
    void normalize() {
        BigInteger numCopy = numerator;
        BigInteger denCopy = denominator;
        if (numCopy.sign() == -1)
            numCopy = -numCopy;
        BigInteger divisor = gcd(numCopy, denCopy);
        numerator /= divisor;
        denominator /= divisor;
    }
    static BigInteger gcd(BigInteger& a, BigInteger& b) {
        BigInteger result = 1;
        while(!a.isNull() && !b.isNull()) {
            if (a.isEven() && b.isEven()) {
                a.divideByTwo();
                b.divideByTwo();
                result.shortMultiply(2);
            } else if (a.isEven()) {
                a.divideByTwo();
            } else if (b.isEven()) {
                b.divideByTwo();
            } else {
                if(a > b) a -= b;
                else b -= a;
            }
        }
        if (b.isNull())
            return result * a;
        return result * b;
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
std::istream& operator>>(std::istream& is, Rational& num) {
    is >> num.numerator;
    return is;
}
template<size_t N, size_t K, bool flag>
struct isPrimeHelper {
    static const bool value = N % K != 0 && isPrimeHelper<N, K + 1, K * K < N>::value;
};

template<size_t N, size_t K>
struct isPrimeHelper<N, K, false> {
    static const bool value = true;
};
template<size_t N>
struct isPrime {
    static const bool value = isPrimeHelper<N, 2, true>::value;
};
template<size_t N>
class Residue {
private:
    int remainder;
    static int BinaryExp(int x, int pow) {
        if (pow == 0) return 1;
        if (pow == 1) return x;
        else if (pow % 2 == 1)
            return x * BinaryExp(x, pow - 1) % static_cast<int>(N);
        int tmp = BinaryExp(x, pow / 2);
        return tmp * tmp % static_cast<int>(N);
    }
public:
    Residue(): remainder(0) {}
    explicit Residue(int x): remainder(x % static_cast<int>(N)) {
        if (remainder < 0)
            remainder += static_cast<int>(N);
    }
    explicit operator int() const {
        return remainder;
    }
    Residue operator-() {
        return Residue(static_cast<int>(N) - remainder);
    }
    Residue& operator+=(const Residue& other) {
        remainder += other.remainder;
        if (remainder >= static_cast<int>(N))
            remainder -= static_cast<int>(N);
        return *this;
    }
    Residue& operator-=(const Residue& other) {
        remainder -= other.remainder;
        if (remainder < 0)
            remainder += static_cast<int>(N);
        return *this;
    }
    Residue& operator*=(const Residue& other) {
        remainder *= other.remainder;
        remainder %= static_cast<int>(N);
        return *this;
    }
    Residue& operator/=(const Residue& other) {
        static_assert(isPrime<N>::value);
        int inverted = BinaryExp(other.remainder, static_cast<int>(N) - 2);
        remainder *= inverted;
        remainder %= static_cast<int>(N);
        return *this;
    }
    Residue operator+(const Residue &other) const {
        Residue result = *this;
        result += other;
        return result;
    }
    Residue operator-(const Residue &other) const {
        Residue result = *this;
        result -= other;
        return result;
    }
    Residue operator*(const Residue &other) const {
        Residue result = *this;
        result *= other;
        return result;
    }
    Residue operator/(const Residue &other) const {
        Residue result = *this;
        result /= other;
        return result;
    }
    bool operator==(const Residue& other) const {
        return remainder == other.remainder;
    }
    bool operator!=(const Residue& other) const {
        return remainder != other.remainder;
    }
};

template<size_t M, size_t N, typename Field = Rational>
class Matrix {
public:
    vector<vector<Field>> table;
public:
    Matrix(): table(M, vector<Field>(N)) {
        if (M == N) {
            for (size_t i = 0; i < M; ++i) {
                table[i][i] = Field(1);
            }
        }
    }
    Matrix(const vector<vector<Field>>& matrix): table(matrix) {}
    Matrix(const vector<vector<int>>& matrix): table(M, vector<Field>(N)) {
        for (size_t i = 0; i < M; ++i) {
            for (size_t j = 0; j < N; ++j) {
                table[i][j] = Field(matrix[i][j]);
            }
        }
    }
    Matrix(const std::initializer_list<std::initializer_list<int>>& init): table(M, vector<Field>(N)) {
        size_t i = 0;
        for (auto& row: init) {
            size_t j = 0;
            for (auto& x: row) {
                table[i][j] = Field(x);
                ++j;
            }
            ++i;
        }
    }
    Matrix& operator+=(const Matrix& other) {
        for (size_t i = 0; i < M; ++i) {
            for (size_t j = 0; j < N; ++j) {
                table[i][j] += other.table[i][j];
            }
        }
        return *this;
    }
    Matrix& operator-=(const Matrix& other) {
        for (size_t i = 0; i < M; ++i) {
            for (size_t j = 0; j < N; ++j) {
                table[i][j] -= other.table[i][j];
            }
        }
        return *this;
    }
    Matrix& operator*=(const Field& a) {
        for (size_t i = 0; i < M; ++i) {
            for (size_t j = 0; j < N; ++j) {
                table[i][j] *= a;
            }
        }
        return *this;
    }
    Matrix& operator*=(const Matrix& other) {
        static_assert(N == M);
        *this = *this * other;
        return *this;
    }
    bool operator==(const Matrix& other) const {
        for (size_t i = 0; i < M; ++i) {
            for (size_t j = 0; j < N; ++j) {
                if (table[i][j] != other[i][j])
                    return false;
            }
        }
        return true;
    }
    bool operator!=(const Matrix& other) const {
        return !(*this == other);
    }
    Field det() const {
        static_assert(N == M);
        vector<vector<Field>> copy = table;
        return gaussianElimination(copy);
    }
    vector<Field>& operator[](size_t index) {
        return table[index];
    }

    const vector<Field>& operator[](size_t index) const {
        return table[index];
    }
    Matrix<N, M, Field> transposed() const {
        Matrix<N, M, Field> result;
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < M; ++j) {
                result[i][j] = table[j][i];
            }
        }
        return result;
    }
    size_t rank() const {
        vector<vector<Field>> copy = table;
        gaussianElimination(copy);
        size_t rank = 0;
        for (size_t i = 0; i < M; ++i) {
            for (size_t j = 0; j < N; ++j) {
                if (copy[i][j] != Field()) {
                    ++rank;
                    break;
                }
            }
        }
        return rank;
    }
    Field trace() const {
        static_assert(N == M);
        Field result;
        for (size_t i = 0; i < M; ++i) {
            result += table[i][i];
        }
        return result;
    }
    Matrix inverted() const {
        Matrix copy = *this;
        copy.invert();
        return copy;
    }
    void invert() {
        static_assert(N == M);
        vector<vector<Field>> extended(M, vector<Field>(2 * M));
        for (size_t i = 0; i < M; ++i)
            for (size_t j = 0; j < M; ++j)
                extended[i][j] = table[i][j];
        for (size_t i = 0; i < M; ++i)
            extended[i][M + i] = Field(1);

        gaussianElimination(extended);
        for (size_t i = 0; i < M; ++i)
            for (size_t j = 0; j < M; ++j)
                table[i][j] = extended[i][M + j];
    }
    vector<Field> getRow(size_t i) const {
        return table[i];
    }
    vector<Field> getColumn(size_t j) const {
        vector<Field> result(M);
        for (size_t i = 0; i < M; ++i) {
            result[i] = table[i][j];
        }
        return result;
    }
private:
    static int nonzero(const vector<vector<Field>>& matrix, int column, int row) {
        int m = static_cast<int>(matrix.size());
        for (int i = row; i < m; ++i) {
            if (matrix[i][column] != Field(0))
                return i;
        }
        return -1;
    }
    static void substractRow(vector<vector<Field>>& matrix, int i, int row, Field x) {
        int n = static_cast<int>(matrix[0].size());
        for (int j = n - 1; j >= 0; --j) {
            matrix[i][j] -= matrix[row][j] * x;
        }
    }
    static Field gaussianElimination(vector<vector<Field>>& matrix) {
        int n = static_cast<int>(matrix[0].size());
        int m = static_cast<int>(matrix.size());
        int row = 0;
        int rank = 0;
        Field det = Field(1);
        for (int column = 0; column < n; ++column) {
            int i = nonzero(matrix, column, row);
            if (i == -1) continue;
            ++rank;
            if (row != i) {
                swap(matrix[row], matrix[i]);
                det = -det;
            }
            Field pivot = matrix[row][column];
            for (int j = n - 1; j >= 0; --j) {
                matrix[row][j] /= pivot;
            }
            det *= pivot;
            for (int i = 0; i < row; ++i) {
                substractRow(matrix, i, row, matrix[i][column]);
            }
            for (int i = row + 1; i < m; ++i) {
                substractRow(matrix, i, row, matrix[i][column]);
            }
            ++row;
            if (row == m) break;
        }
        if (rank < n)
            det = Field(0);
        return det;
    }
};
template<size_t M, size_t N, typename Field = Rational>
Matrix<M, N, Field> operator+(const Matrix<M, N, Field>& left, const Matrix<M, N, Field>& right) {
    Matrix<M, N, Field> result = left;
    result += right;
    return result;
}
template<size_t M, size_t N, typename Field = Rational>
Matrix<M, N, Field> operator-(const Matrix<M, N, Field>& left, const Matrix<M, N, Field>& right) {
    Matrix<M, N, Field> result = left;
    result -= right;
    return result;
}
template<size_t M, size_t N, typename Field = Rational>
Matrix<M, N, Field> operator*(const Field& left, const Matrix<M, N, Field>& right) {
    Matrix<M, N, Field> result = right;
    result *= left;
    return result;
}
template<size_t M, size_t K, size_t N, typename Field = Rational>
Matrix<M, N, Field> operator*(const Matrix<M, K, Field>& left, const Matrix<K, N, Field>& right) {
    Matrix<M, N, Field> result;
    for (int i = 0; i < static_cast<int>(M); ++i) {
        for (int j = 0; j < static_cast<int>(N); ++j) {
            result[i][j] = Field();
            for (int k = 0; k < static_cast<int>(K); ++k) {
                result[i][j] += left[i][k] * right[k][j];
            }
        }
    }
    return result;
}
template<size_t N, typename Field = Rational>
using SquareMatrix = Matrix<N, N, Field>;