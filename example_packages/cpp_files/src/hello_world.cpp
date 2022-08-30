#include <vector>
#include <algorithm>

extern "C" {

int intvec_maxval(int* array, size_t n){

  std::vector<int> vec(array, array + n);
  
  return *(std::max_element(vec.begin(), vec.end()));
  
}

}