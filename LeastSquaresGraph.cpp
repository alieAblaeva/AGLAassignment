#include <iostream>
#include <vector>
#include <exception>
#include <fstream>
#include <iomanip>
#include <valarray>
#include <cstdlib>
#include <cstdio>
#include <random>

using namespace std;

class Matrix{
public:
    int rows;
    int columns;
    vector<vector<double>> elements;
public:
    Matrix(){}
    Matrix(double rows, double columns){
        this->rows = rows;
        this->columns = columns;
        this->elements.resize(rows);
        for(int i = 0; i<rows; i++){
            this->elements[i].resize(columns);
        }
    }
    friend istream& operator>>(istream &input, Matrix &matrix){
        input>>matrix.rows>>matrix.columns;
        matrix.elements.resize(matrix.rows);
        for(double i = 0; i<matrix.rows; i++){
            matrix.elements[i].resize(matrix.columns);
            for(int j = 0; j<matrix.columns; j++){
                input>>matrix.elements[i][j];
            }
        }
        return input;
    }
    friend ostream& operator<<(ostream &output, Matrix &matrix){
        for(int i = 0; i<matrix.rows; i++){
            for(int j = 0; j<matrix.columns; j++){
                if (abs(matrix.elements[i][j]) < 1e-10){
                    output<<0.00<<" ";
                }
                else
                    output<<fixed<<setprecision(4)<<matrix.elements[i][j] << " ";
            }
            output<<endl;
        }
        return output;
    }
    Matrix operator = (const Matrix &anotherMatrix){
        this->rows = anotherMatrix.rows;
        this->columns = anotherMatrix.columns;
        this->elements.resize(this->rows);
        for(int i = 0; i<this->rows; i++){
            this->elements[i].resize(columns);
            for(int j = 0; j<this->columns; j++){
                this->elements[i][j] = anotherMatrix.elements[i][j];
            }
        }
        return *this;
    }
    Matrix operator + (const Matrix &anotherMatrix){
        if (this->columns != anotherMatrix.columns || this->rows != anotherMatrix.rows) {
            throw invalid_argument("Error: the dimensional problem occurred\n");
        }
        Matrix temp;
        temp = *this;
        for (int i = 0; i < this->rows; i++) {
            for (int j = 0; j < this->columns; j++) {
                temp.elements[i][j] += anotherMatrix.elements[i][j];
            }
        }
        return temp;
    }
    Matrix operator-(const Matrix &anotherMatrix){
        if (this->columns != anotherMatrix.columns || this->rows != anotherMatrix.rows) {
            throw invalid_argument("Error: the dimensional problem occurred\n");
        }
        Matrix temp = *this;
        for (int i = 0; i < this->rows; i++) {
            for (int j = 0; j < this->columns; j++) {
                temp.elements[i][j] -= anotherMatrix.elements[i][j];
            }
        }
        return temp;
    }
    Matrix operator *(const Matrix &anotherMatrix){
        if (this->columns != anotherMatrix.rows) {
            throw invalid_argument("Error: the dimensional problem occurred\n");
        }
        Matrix temp(this->rows, anotherMatrix.columns);
        for (int i = 0; i < this->rows; i++) {
            for (int j = 0; j < anotherMatrix.columns; j++) {
                for (int k = 0; k < this->columns; k++) {
                    temp.elements[i][j] += this->elements[i][k] * anotherMatrix.elements[k][j];
                }
            }
        }
        return temp;
    }
    Matrix transpose(){
        Matrix temp(this->columns, this->rows);
        for(int i = 0; i<this->columns; i++){
            for(int j = 0; j<this->rows; j++){
                temp.elements[i][j] = this->elements[j][i];
            }
        }
        return temp;
    }

    Matrix augmented(){
        Matrix matrix(rows, columns*2);
        for(int j = 0; j<columns*2; j++){
            for(int i = 0; i<rows; i++){
                if(j<columns)
                    matrix.elements[i][j] = elements[i][j];
                else{
                    if(i+columns == j)
                        matrix.elements[i][j] = 1;
                    else
                        matrix.elements[i][j] = 0;
                }
            }
        }
        return matrix;
    }
};

class ColumnVector: public Matrix {
public:
    int dimension;

public:
    double operator[](int i) {
        return elements[i][0];
    }
    ColumnVector() {}

    ColumnVector(int dimension) {
        this->dimension = dimension;
        this->rows = dimension;
        this->columns = 1;
        this->elements.resize(dimension);
        for(int i = 0; i<dimension; i++){
            this->elements[i].resize(1);
        }
    }
    /**
     * Method counts the norm / length of a vector.
     * @return norm
     */
    int norm() {
        int sum;
        for (int i = 0; i < elements.size(); i++) {
            sum += elements[i][0] * elements[i][0];
        }
        return sqrt(sum);
    }

};

class SquareMatrix: public Matrix{
public:
    SquareMatrix() = default;
    SquareMatrix(int size): Matrix(size, size){}
    friend istream& operator>>(istream &input, SquareMatrix &matrix){
        input>>matrix.rows;
        matrix.columns = matrix.rows;
        matrix.elements.resize(matrix.rows);
        for(int i = 0; i<matrix.rows; i++){
            matrix.elements[i].resize(matrix.columns);
            for(int j = 0; j<matrix.columns; j++){
                input>>matrix.elements[i][j];
            }
        }
        return input;
    }
    SquareMatrix operator=(const Matrix& other){
        Matrix::operator=(other);
        return *this;
    }
    int getSize(){
        return this->elements.size();
    }

};

class IdentityMatrix: public SquareMatrix{
public:
    IdentityMatrix(int size): SquareMatrix(size) {
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                if(i == j)
                    elements[i][j] = 1;
                else
                    elements[i][j] = 0;
            }
        }
    }

};

class EliminationMatrix: public SquareMatrix{
public:
    EliminationMatrix(int size, Matrix matrix, int row, int col): SquareMatrix(size){
        row--; col--;
        for(int i = 0; i<size; i++){
            for(int j = 0; j<size; j++){
                if(i == j)
                    elements[i][j] = 1;
                else
                    elements[i][j] = 0;
            }
        }
        double el = -1 * matrix.elements[row][col]/matrix.elements[col][col];
        elements[row][col] = el;
    }
};

class PermutationMatrix: public SquareMatrix{
public:
    PermutationMatrix(int size, int row1, int row2): SquareMatrix(size){
        row1--; row2--;
        for(int i = 0; i<size; i++){
            for(int j = 0; j<size; j++){
                if(i == row1 && j == row2 || i == row2 && j == row1 || (i == j && i!=row1 && i!= row2))
                    elements[i][j] = 1;
                else
                    elements[i][j] = 0;
            }
        }
    }
    PermutationMatrix operator=(PermutationMatrix& other){
        Matrix::operator=(other);
        return *this;
    }
};

Matrix Inversed(Matrix *a){
    for(int row = 0; row<a->rows; row++){
        double pivot = a->elements[row][row];
        int pivotRow = row;
        for(int r = row+1;r<a->rows; r++){
            if(abs(a->elements[r][row])>abs(pivot)){
                pivot = a->elements[r][row];
                pivotRow = r;
            }
        }
        if(pivot == 0){
            continue;
        }
        if(pivotRow!= row){
            PermutationMatrix p(a->rows, row+1, pivotRow+1);
            *a = p*(*a);
        }
        for(int r = row+1; r<a->rows; r++){
            if(a->elements[r][row] == 0)
                continue;
            EliminationMatrix e(a->rows, *a, r+1, row+1);
            *a = e*(*a);
        }
    }
    for(int row = a->rows-1; row>=0; row--){
        for(int r = row-1; r>=0; r--){
            EliminationMatrix e(a->rows, *a, r+1, row+1);
            *a = e*(*a);
        }
    }
    for(int i = 0; i<a->rows; i++){
        double piv = a->elements[i][i];
        for(int j = 0; j<a->rows*2; j++){
            a->elements[i][j]/=piv;
        }
    }
    SquareMatrix mat(a->rows);
    for(int i = 0; i<a->rows; i++){
        for(int j = a->rows; j<a->rows*2; j++)
            mat.elements[i][j-a->rows] = a->elements[i][j];
    }
    return mat;
}

#ifdef WIN32
#define GNUPLOT_NAME "C:\\gnuplot\\bin\\gnuplot -persist"
#else
#define GNUPLOT_NAME "gnuplot -persist"
#endif
int main() {
#ifdef WIN32
    FILE* pipe = _popen(GNUPLOT_NAME, "w");
#else
    FILE* pipe = popen(GNUPLOT_NAME, "w");
#endif
    default_random_engine _random{std::random_device{}()};
    uniform_real_distribution<double> interval(3, 20);
    int n;
    cin>>n;
    vector<pair<double, double>> input;
    for(int i = 0; i<n; i++){
        int x = interval(_random);
        int y = interval(_random);
        cout<<x<<" "<<y<<endl;
        input.push_back(make_pair(x, y));
    }
    int degree = 4;
    Matrix A = *new Matrix(n, degree+1);
    ColumnVector b = *new ColumnVector(n);
    for(int i = 0; i<n; i++){
        A.elements[i][0] = 1;
        for(int j = 0; j<degree; j++){
            A.elements[i][j+1] = pow(input[i].first, j+1);
        }
        b.elements[i][0] = input[i].second;
    }
    cout<<"A:"<<endl<<A;
    Matrix A_T = A.transpose();
    Matrix A_TA = A_T*A;
    cout<<"A_T*A:"<<endl<<A_TA;
    Matrix augmented = A_TA.augmented();
    Matrix A_TA_inv = Inversed(&augmented);
    cout<<"(A_T*A)^-1:"<<endl<<A_TA_inv;
    Matrix multiplication = A_T*b;
    cout<<"A_T*b:"<<endl<<multiplication;
    Matrix x = A_TA_inv*multiplication;
    cout<<"x~:"<<endl<<x;
    double k0 = x.elements[0][0], k1 = x.elements[1][0], k2 = x.elements[2][0], k3 = x.elements[3][0], k4 = x.elements[4][0];
    fprintf(pipe, "set yrange [0:30]\n");
    fprintf(pipe, "plot [0 : 30] [0 : 30] %lf*x**4 + %lf*x**3 + %lf*x**2 + %lf*x**1 + %lf*x**0 , '-' using 1:2 with points\n", k4, k3, k2, k1, k0);
    for(int i = 0; i<n;i++) {
        fprintf(pipe, "%f\t%f\n", input[i].first, input[i].second);
    }
    fprintf(pipe, "e\n");
    fflush(pipe);
#ifdef WIN32
    _pclose(pipe);
#else
    pclose(pipe);
#endif
    return 0;
}
