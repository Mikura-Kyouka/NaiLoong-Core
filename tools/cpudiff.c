#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    FILE *file1;
    FILE *file2;

    if (argc != 3) {
        printf("Usage: %s <file1> <file2>\n", argv[0]);
        return 1;
    }

    file1 = fopen(argv[1], "r");
    if (file1 == NULL) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    file2 = fopen(argv[2], "r");
    if (file2 == NULL) {
        printf("Error opening file: %s\n", argv[2]);
        fclose(file1);
        return 1;
    }

    long long line_num1 = 0, line_num2 = 0;

    long long rf_wen1, pc1, rf_num1, wb_data1;
    long long rf_wen2, pc2, rf_num2, wb_data2;

    int diff_count = 0;

    while(1)
    {
        do {
            if(fscanf(file1, "%llx %llx %llx %llx\n", &rf_wen1, &pc1, &rf_num1, &wb_data1) == 4) {
                line_num1++;
            } else {
                printf("End of file1 reached at line %lld\n", line_num1);
                printf("File2 read lines: %lld\n", line_num2);
                fclose(file1);
                fclose(file2);
                return 0;
            }
        } while(rf_num1 == 0);

        do {
            if(fscanf(file2, "%llx %llx %llx %llx\n", &rf_wen2, &pc2, &rf_num2, &wb_data2) == 4) {
                line_num2++;
            } else {
                printf("End of file2 reached at line %lld\n", line_num2);
                printf("File1 read lines: %lld\n", line_num1);
                fclose(file1);
                fclose(file2);
                return 0;
            }
        } while(rf_num2 == 0);

        if (rf_num1 != rf_num2 || wb_data1 != wb_data2) {
            printf("Different at line %lld <-> %lld, file1: %08llx %02llx %08llx, file2: %08llx %02llx %08llx\n", 
                  line_num1, line_num2, pc1, rf_num1, wb_data1, pc2, rf_num2, wb_data2);
            diff_count++;
            if (diff_count >= 20) {
                printf("Too many diffs, exit now.\n");
                break;
            }
        }
    }

    fclose(file1);
    fclose(file2);
    return 0;
}