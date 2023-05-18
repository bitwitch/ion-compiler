#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>
char ((board[3])[3]) = {0};
void draw_board(void) {
    printf("   0 1 2\n");
    printf("0 |%c|%c|%c|\n", (board[0][0] ? board[0][0] : ' '), (board[0][1] ? board[0][1] : ' '), (board[0][2] ? board[0][2] : ' '));
    printf("1 |%c|%c|%c|\n", (board[1][0] ? board[1][0] : ' '), (board[1][1] ? board[1][1] : ' '), (board[1][2] ? board[1][2] : ' '));
    printf("2 |%c|%c|%c|\n", (board[2][0] ? board[2][0] : ' '), (board[2][1] ? board[2][1] : ' '), (board[2][2] ? board[2][2] : ' '));
}
bool make_move(int row, int col, char side) {
    if ((((((row) < (0)) || ((row) > (2))) || ((col) < (0))) || ((col) > (2))) || ((int)(board[row][col]))) {
        return false;
    } else {
        board[row][col] = side;
        return true;
    }
}
void make_random_move(char side) {
    int row;
    int col;
    do {
        row = (rand()) % (3);
        col = (rand()) % (3);
    } while(!(make_move(row, col, side)));
}
bool no_moves(void) {
    for (int row = 0; (row) < (3); row++) {
        for (int col = 0; (col) < (3); col++) {
            if (((int)(board[row][col])) == (0)) {
                return false;
            }
        }
    }
    return true;
}
bool winner(char side) {
    for (int row = 0; (row) < (3); row++) {
        if ((((board[row][0]) == (side)) && ((board[row][1]) == (side))) && ((board[row][2]) == (side))) {
            return true;
        }
    }
    for (int col = 0; (col) < (3); col++) {
        if ((((board[0][col]) == (side)) && ((board[1][col]) == (side))) && ((board[2][col]) == (side))) {
            return true;
        }
    }
    if ((((board[0][0]) == (side)) && ((board[1][1]) == (side))) && ((board[2][2]) == (side))) {
        return true;
    }
    if ((((board[0][2]) == (side)) && ((board[1][1]) == (side))) && ((board[2][0]) == (side))) {
        return true;
    }
    return false;
}
int main(int argc, char (*(*argv))) {
    draw_board();
    while(true) {
        int row;
        int col;
        printf("Your move. Enter row,col: ");
        scanf("%d,%d", &(row), &(col));
        while(!(make_move(row, col, 'x'))) {
            printf("Invalid move. Try again. Enter row,col: ");
            scanf("%d,%d", &(row), &(col));
        }
        draw_board();
        if (no_moves()) {
            printf("Tie game!");
            break;
        }
        if (winner('x')) {
            printf("You win!");
            break;
        }
        make_random_move('o');
        draw_board();
        if (no_moves()) {
            printf("Tie game!");
            break;
        }
        if (winner('o')) {
            printf("You lose!");
            break;
        }
    }
    return 0;
}
