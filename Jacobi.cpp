#include <iostream>
#include <vector>
#include <exception>
#include <fstream>
#include <iomanip>
#include <valarray>
#include <cstdlib>

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
                    output<<"0.0000"<<" ";
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
    /**
     * Overloaded operator [] to reach elements of vector.
     * @param i index of needed element
     * @return ith element of vector
     */
    double operator[](int i) {
        return elements[i][0];
    }
    /**
     * Constructor with no parameters.
     */
    ColumnVector() {}

    /**
     * Constructor with given dimension.
     * @param dimension size of vector
     */
    ColumnVector(int dimension) {
        this->dimension = dimension;
        this->rows = dimension;
        this->columns = 1;
        this->elements.resize(dimension);
        for(int i = 0; i<dimension; i++){
            this->elements[i].resize(1);
        }
    }
    friend istream& operator>>(istream &input, ColumnVector &vector){
        input>>vector.rows;
        vector.columns = 1;
        vector.elements.resize(vector.rows);
        for(int i = 0; i<vector.rows; i++){
            vector.elements[i].resize(1);
            for(int j = 0; j<vector.columns; j++){
                input>>vector.elements[i][j];
            }
        }
        return input;
    }
    /**
     * Method counts the norm / length of a vector.
     * @return norm
     */
    double norm() {
        double sum = 0;
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
int main() {
    SquareMatrix a;
    cin>>a;
    ColumnVector b;
    cin>>b;
    double epsilon; cin>>epsilon;
    SquareMatrix d(a.getSize());
    for(int i =0; i<a.rows; i++){
        for(int j = 0; j<a.columns; j++){
            if(i == j)
                d.elements[i][j] = a.elements[i][j];
            else {
                if (a.elements[i][i] > a.elements[i][j])
                    d.elements[i][j] = 0;
                else {
                    cout << "The method is not applicable!";
                    return 0;
                }
            }
        }
    }
    Matrix dAugmented = d.augmented();
    Matrix dInversed = Inversed(&dAugmented);
    IdentityMatrix i(a.getSize());

    Matrix alpha = i - dInversed*a;
    Matrix beta = dInversed*b;
    Matrix x = beta;
    cout<<"alpha:"<<endl<<alpha;
    cout<<"beta:"<<endl<<beta;
    cout<<"x(0):"<<endl<<x;
    double e = 100; ColumnVector vectorE(a.getSize());
    Matrix x_i = x;
    int j = 0;
    while(e>epsilon){
        j++;
        x = x_i + dInversed*(b-a*x_i);
        for(int i = 0; i<a.getSize(); i++){
            vectorE.elements[i][0] = x.elements[i][0] - x_i.elements[i][0];
        }

        e = vectorE.norm();
        cout<<"e: "<<e<<endl;
        cout<<"x("<<j<<"):"<<endl<<x;
        x_i = x;
    }
    return 0;
}
