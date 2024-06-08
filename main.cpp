#include <iostream>
#include <math.h>
#include <vector>
#include <algorithm>
#include <cmath>
#include <numeric>
using namespace std;

void the_Hurwitz_criterion (vector<double> alt1, vector<double> alt2, vector<double> alt3){
    vector<vector<double>> alternatives = {alt1, alt2, alt3};

    cout << "Alternatives and their benefits: " << endl;
    for(int i = 1; i <= 3; i++){
        cout<< "alternative " << i << ": " << "ui(v) = " <<
            *min_element(alternatives[i-1].begin(), alternatives[i-1].end())
            <<  ", " << "ui(m) = " <<
            *max_element(alternatives[i-1].begin(), alternatives[i-1].end()) << endl;
    }
    double a = 0.37;
    cout << endl << "Calculation according to the Hurwitz criterion for each alternative: " << endl;
    for(int i = 1; i <= 3; i++){
        double cur_max = *max_element(alternatives[i-1].begin(), alternatives[i-1].end());
        double cur_min = *min_element(alternatives[i-1].begin(), alternatives[i-1].end());
        cout<< "alternative " << i << ": " << 'u' << i << "(g) = " << a << " * " << cur_max
        << " + (1 - " << a <<')'<< " * " << cur_min << "= " << a*cur_max+(1-a)*cur_min << endl;

    }
}
void Laplace_criterion (vector<double> alt1, vector<double> alt2, vector<double> alt3) {
    vector<vector<double>> alternatives = {alt1, alt2, alt3};
    for (int i = 1; i <= 3; i++) {
        double sum = 0;
        cout << "alternative " << i << ": ";
        for (int k = 0; k <= 2; k++) {
            cout << alternatives[i - 1][k];
            sum += alternatives[i - 1][k];
            if (k == 2) { cout << " / 3 = " << sum << " / 3 = " << sum / 3 << endl; }
            else {
                { cout << " + "; }
            }
        }
    }
}
void Savage_criterion (vector<double> alt1, vector<double> alt2, vector<double> alt3){
    vector<vector<double>> alternatives = {alt1, alt2, alt3};
    vector<double> scs = {alternatives[0][0],alternatives[0][1],alternatives[0][2]};
    for(int i = 0; i <=2; i++){
        for(int k = 0; k <= 2; k++){
            if(alternatives[i][k] > scs[k]){
                scs[k] = alternatives[i][k];
            }
        }
    }
    cout << "maximum gain for each possible scenario: " << endl;
    for(int i = 0; i <=2;i++){
        cout <<"scenario " << i+1 << ": "<< scs[i] << endl;
    }

    cout << endl << "alternatives   "<< "                  scenarios                  " << endl;
    cout << "                      1              2              3       " << endl;
    for(int i = 0; i <=2; i++){
        cout << i+1 << "                 " ;
        for(int k = 0; k <= 2; k++){
            cout << scs[k] << '-' << alternatives[i][k] << '=' << scs[k]-alternatives[i][k] << "     ";
        }
        cout << endl;
    }

}
vector<double> Bayes_Laplace_criteria(vector<double> alt1, vector<double> alt2, vector<double> alt3){
    vector<vector<double>> alternatives = {alt1, alt2, alt3};
    vector<double> ps = {0.3, 0.5, 0.2};
    vector<double> ub;
    for(int i = 0; i <=2; i++){
        double u = 0;
        cout<< "alternative " << i+1 << ": " << "u" << i+1 << "(b) = " ;
        for(int k = 0; k <= 2; k++){
            cout << ps[k] << '*' << alternatives[i][k];
            u += ps[k]*alternatives[i][k];
            if (k == 2){cout << " = " << u << endl; ub.push_back(u); }
            else{ cout << " + ";}
        }
    }
    return ub;
}
void Hodges_Lehman_criterion (vector<double> alt1, vector<double> alt2, vector<double> alt3){
    vector<vector<double>> alternatives = {alt1, alt2, alt3};
    double b = 0.8;
    vector<double> ub = Bayes_Laplace_criteria(alt1, alt2, alt3);
    for(int i = 0; i <=2; i++) {
        cout << endl << "alternative " << i + 1 << ": " << "u" << i + 1 << "(hl) = " << b <<" * "
        << ub[i] << " + " << "(1 - " << b << ") * "<< *min_element(alternatives[i].begin(),
                                                                   alternatives[i].end())
        << " = "<< b*ub[i]+(1-b)**min_element(alternatives[i].begin(), alternatives[i].end());
    }
}

double calculate_average_v(double v1, double v2){
    return (v1+v2)/2;
}
pair<int,double> max_iterator(vector<double> vec){
    double max = vec[0];
    int itr = 0;
    for(int i = 0; i < vec.size(); i++){
        if(vec[i] > max){
            max = vec[i];
            itr = i;
        }
    }
    pair<int,double> a = {itr,max};
    return a;
}
pair<int,double> min_iterator(vector<double> vec){
    double min = vec[0];
    int itr = 0;
    for(int i = 0; i < vec.size(); i++){
        if(vec[i] < min){
            min = vec[i];
            itr = i;
        }
    }
    pair<int,double> a = {itr,min};
    return a;
}
vector<double> new_previous(vector<double> prev, vector<double> vec){
    vector<double> new_prev;
    for(int i = 0; i < vec.size(); i++){
        new_prev.push_back(prev[i]+vec[i]);
    }
    return new_prev;
}

void Browns_method (vector<vector<double>> A, vector<vector<double>> B, int c){
    int current_chose_j = 1;
    int current_chose_i = 1;
    double v_top;
    double v_button;
    vector<double> v;
    vector<int> j;
    vector<int> i;
    vector<double> previous_a = {0,0,0,0,0};
    vector<double> previous_b = {0,0,0,0};

    vector<int> count_j = {0,0,0,0};
    vector<int> count_i = {0,0,0,0,0};

    for(int k = 1; k <= c; k++){

        previous_b = new_previous(previous_b,B[current_chose_j-1]);
        //pair<int,double> max_b = max_iterator(previous_b);
        current_chose_i = max_iterator(previous_b).first+1;
        j.push_back(current_chose_i);
        v_top = max_iterator(previous_b).second/k;
        i.push_back(current_chose_j);
        previous_a = new_previous(previous_a,A[current_chose_i-1]);
        current_chose_j = min_iterator(previous_a).first+1;
        v_button =  min_iterator(previous_a).second/k;
        v.push_back(calculate_average_v(v_top,v_button));
    }
    for(int s : j){
        count_j[s-1]++;
    }
    for(int s : i){
        count_i[s-1]++;
    }
    double sum_i = accumulate(count_i.begin(), count_i.end(),0);
    double sum_j = accumulate(count_j.begin(), count_j.end(),0);

    ///Вивід потрібних нам даних
    cout << endl << "Cost of game: " << v[c-1];
    cout << endl << "x* = {";
    for(double s : count_j){
        double xj = s/sum_j;
        cout << xj << "; ";
    }
    cout << "}" << endl << "x: ";
    for(double s : count_j){
        cout << s << "; ";
    }
    cout << endl << "y* = {";
    for(double s : count_i){
        double xi = s/sum_i;
        cout << xi << "; ";
    }
    cout << "}" << endl << "y: ";
    for(double s : count_i){
        cout << s << "; ";
    }
    cout << endl;
    for(double s : previous_b){
        cout << s << "; ";
    }
}

vector<double> operator * (vector<vector<double>>& matrix, vector<double>& w0) {
    vector<double> w1(matrix.size(), 0);

    for(int i = 0; i < matrix.size(); i++){
        for(int j = 0; j < matrix[i].size(); j++){
            w1[i] += matrix[i][j] * w0[j];
        }
    }
    return w1;
}
void sum_method_lines (vector<vector<double>> matrix, vector<double> w0, int number_of_itr){
    number_of_itr++;
    vector<double> y = matrix * w0;
    vector<double> w_new(y.size(), 0);
    double sum = accumulate(y.begin(), y.end(), 0.0);
    for(int i = 0; i < y.size(); i++){
        w_new[i] = y[i] / sum;
    }
    bool similar = true;
    cout << endl << "iteration " << number_of_itr << ": ";
    cout << endl << "y(" << number_of_itr-1 << "): ";
    for(double s : y){
        cout << round(s * 10000) / 10000 << "; ";
    }
    cout << endl << "w(" << number_of_itr << "): ";
    for(double s : w_new){
        cout << round(s * 10000) / 10000 << "; ";
    }
    cout << endl;
    for(int k = 0; k < w0.size(); k++){
        if(round(w0[k] * 10000) / 10000 != round(w_new[k] * 10000) / 10000) {similar = false;};
    }
    if (!similar){sum_method_lines(matrix,w_new,number_of_itr);}

}

double sumVector(vector<double>& vec) {
    double sum = 0;
    for (double val : vec) {
        sum += val;
    }
    return sum;
}
void print_duel (vector<vector<pair<double,double>>> matrix, int n){
    cout << " |";
    for(int i = 1; i <= n/4; i++){cout << "       " << i << "      ";}
    cout << endl;
    for(int i = 0; i < n; i ++){
        cout << i+1;
        for(int j = 0; j < n/4; j++){
            cout << "| (" <<  round(matrix[i][j].first * 100) / 100 << ";" << round(matrix[i][j].second * 100) / 100 << ") ";
        }
        cout << endl;
    }
    cout << " |";
    for(int i = (n/4)+1; i <= (n/4)*2; i++){cout << "       " << i << "      ";}
    cout << endl;
    for(int i = 0; i < n; i ++){
        cout << i+1;
        for(int j = n/4; j < (n/4)*2; j++){
            cout << "| (" <<  round(matrix[i][j].first * 100) / 100 << ";" << round(matrix[i][j].second * 100) / 100 << ") ";
        }
        cout << endl;
    }
    cout << " |";
    for(int i = ((n/4)*2)+1; i <= (n/4)*3; i++){cout << "       " << i << "      ";}
    cout << endl;
    for(int i = 0; i < n; i ++){
        cout << i+1;
        for(int j = (n/4)*2; j < (n/4)*3; j++){
            cout << "| (" <<  round(matrix[i][j].first * 100) / 100 << ";" << round(matrix[i][j].second * 100) / 100 << ") ";
        }
        cout << endl;
    }
    cout << " |";
    for(int i = ((n/4)*3)+1; i <= n; i++){cout << "       " << i << "      ";}
    cout << endl;
    for(int i = 0; i < n; i ++){
        cout << i+1;
        for(int j = (n/4)*3; j < n; j++){
            cout << "| (" <<  round(matrix[i][j].first * 100) / 100 << ";" << round(matrix[i][j].second * 100) / 100 << ") ";
        }
        cout << endl;
    }
}
void low_price_duel(vector<vector<pair<double,double>>> matrix, int n){
    vector<double> max_i (14,0);
    vector<double> max_j (14,0);
    vector<double> min_j (14,0);
    for(int i = n; i < matrix.size(); i++){
        for(int j = n; j < matrix.size(); j++){
            if(matrix[i][j].first > max_i[i-n]){max_i[i-n] = matrix[i][j].first;}
            if(matrix[i][j].second > max_j[i-n]){max_j[i-n] = matrix[i][j].second;}
            if(matrix[i][j].second < min_j[i-n]){min_j[i-n] = matrix[i][j].second;}

        }
    }

    cout << endl << "Lower game prices player 1: " << *min_element(max_i.begin(), max_i.end());
    cout << endl << "Lower game prices player 2: " << *min_element(max_j.begin(), max_j.end());
    cout << endl << "Top game prices player 2: " << *min_element(min_j.begin(), min_j.end())*-1;

}
void probabilities (vector<vector<pair<double,double>>> matrix, int n){
    vector<double> sums_i (14,0);
    vector<double> sums_j (14,0);
    vector<double> prob_i;
    vector<double> prob_j;
    for(int i = n; i < matrix.size(); i++){
        for(int j = n; j < matrix.size(); j++){
            if(matrix[i][j].first > 0){
                sums_i[i-n] += matrix[i][j].first;
            }else {
                sums_i[i-n] += matrix[i][j].first*-1;
            }
            if(matrix[i][j].second > 0){
                sums_j[i-n] += matrix[i][j].second;
            }else {
                sums_j[i-n] += matrix[i][j].second*-1;
            }
        }
    }
    double sum_i = sumVector(sums_i);
    double sum_j = sumVector(sums_j);
    for(int i = 0; i < 14; i ++){
        prob_i.push_back(sums_i[i]/sum_i);
        prob_j.push_back(sums_j[i]/sum_j);
    }
    cout << endl << "probabilities player 1: " << endl << "{";
    for(double s: prob_i){
        cout << round(s * 100) / 100 <<"; ";
    }
    cout << "}";
    cout << endl << "sum: " << sumVector(prob_i)<< "(for check)";
    cout << endl << "probabilities player 2: " << endl << "{";
    for(double s: prob_j){
        cout << round(s * 100) / 100 <<"; ";
    }
    cout << "}";
    cout << endl << "sum: " << sumVector(prob_j) << "(for check)";
}
void loud_duel (double n){
    pair<double,double> pos;
    vector<pair<double,double>> row;
    vector<vector<pair<double,double>>> matrix;
    for(int i = 0; i < n; i ++){
        for(int j = 0; j < n; j++){
            if(i<j){
                pos.first = (i+1)/n;
                pos.second = ((1-(i+1)/n)*((i+j+2))/(2*n));
            }
            else if(i>j){
                pos.first = ((1-(j+1)/n)*((i+j+2))/(2*n));
                pos.second = (j+1)/n;
            }
            else if(i==j){
                pos.first = ((i+1)*(i+1-n))/(n*n);
                pos.second = ((j+1)*(j+1-n))/(n*n);
            }
            row.push_back(pos);
        }
        matrix.push_back(row);
        row.clear();
    }
    //print_duel (matrix, n);
    //low_price_duel(matrix, 6);
    probabilities(matrix, 6);
}


int main() {
   double n = 20;
    loud_duel(n);

    return 0;
}
