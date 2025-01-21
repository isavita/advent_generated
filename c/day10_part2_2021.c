
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE_LENGTH 1024
#define MAX_SCORES 100

int checkAndCompleteLine(const char *line, long long *score);
void quickSort(long long arr[], int low, int high);
int partition(long long arr[], int low, int high);
int getClosingCharValue(char openingChar);

int main() {
  FILE *file = fopen("input.txt", "r");
  if (!file) {
    perror("Error opening file");
    return 1;
  }

  long long scores[MAX_SCORES];
  int scoreCount = 0;
  char line[MAX_LINE_LENGTH];
    long long score;
  while (fgets(line, sizeof(line), file)) {
      if (checkAndCompleteLine(line,&score)) {
          if (scoreCount < MAX_SCORES){
              scores[scoreCount++] = score;
          }
      }
  }

  fclose(file);
    quickSort(scores, 0, scoreCount - 1);
    
  printf("%lld\n", scores[scoreCount/2]);

  return 0;
}

int checkAndCompleteLine(const char *line, long long *score) {
    char stack[MAX_LINE_LENGTH];
    int stackTop = -1;
    *score = 0;
  for (int i = 0; line[i] != '\0' && line[i] != '\n'; i++) {
        char c = line[i];
        if (c == '(' || c == '[' || c == '{' || c == '<') {
            stack[++stackTop] = c;
        } else if (c == ')' || c == ']' || c == '}' || c == '>') {
            if (stackTop == -1)
                return 0;
                
            char top = stack[stackTop--];
            if ((c == ')' && top != '(') || (c == ']' && top != '[') ||
              (c == '}' && top != '{') || (c == '>' && top != '<')) {
                return 0;
            }
        }
    }
    if (stackTop == -1) return 0;
    
    while (stackTop >= 0){
        *score = *score * 5 + getClosingCharValue(stack[stackTop--]);
    }
    return 1;
}

int getClosingCharValue(char openingChar) {
  switch (openingChar) {
    case '(':
      return 1;
    case '[':
      return 2;
    case '{':
      return 3;
    case '<':
      return 4;
    default:
      return 0;
  }
}


void quickSort(long long arr[], int low, int high) {
    if (low < high) {
        int pi = partition(arr, low, high);
        quickSort(arr, low, pi - 1);
        quickSort(arr, pi + 1, high);
    }
}

int partition(long long arr[], int low, int high) {
    long long pivot = arr[high];
    int i = (low - 1);

    for (int j = low; j <= high - 1; j++) {
        if (arr[j] < pivot) {
            i++;
            long long temp = arr[i];
            arr[i] = arr[j];
            arr[j] = temp;
        }
    }
    long long temp = arr[i + 1];
    arr[i + 1] = arr[high];
    arr[high] = temp;
    return (i + 1);
}
