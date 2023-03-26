#include <Rts.h>
#include <stdio.h>

static PauseToken * pause_token;

void wait_for_enter() {
  int ch;

  while (ch != EOF && ch != '\n') {
    ch = fgetc(stdin);
  }

  clearerr(stdin);
}

void block_on_input() {
  pause_token = rts_pause();
  wait_for_enter();
  rts_resume(pause_token);
}
