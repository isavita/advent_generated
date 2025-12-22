
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_STATES 100
#define TAPE_SIZE 20000
#define TAPE_MID (TAPE_SIZE/2)

typedef struct{
    int write;
    int move;
    char nextState;
} Command;

typedef struct{
    Command cmds[2];
} State;

static int parse(const char *path, char *initState, int *steps, State states[MAX_STATES]){
    FILE *f=fopen(path,"r");
    if(!f){perror("open");exit(1);}
    char line[128];
    fgets(line,sizeof line,f);
    *initState=line[strlen(line)-3];
    fgets(line,sizeof line,f);
    sscanf(line,"Perform a diagnostic checksum after %d steps.",steps);
    while(fgets(line,sizeof line,f)){
        if(strlen(line)<=1)continue;
        char sid;
        sscanf(line,"In state %c:",&sid);
        for(int i=0;i<2;i++){
            fgets(line,sizeof line,f);
            int w;
            fgets(line,sizeof line,f);
            sscanf(line,"    - Write the value %d.",&w);
            int mv;
            fgets(line,sizeof line,f);
            mv=strstr(line,"right")?1:-1;
            char ns;
            fgets(line,sizeof line,f);
            sscanf(line,"    - Continue with state %c.",&ns);
            states[sid-'A'].cmds[i].write=w;
            states[sid-'A'].cmds[i].move=mv;
            states[sid-'A'].cmds[i].nextState=ns;
        }
    }
    fclose(f);
    return 0;
}

int main(int argc, const char * argv[]){
    @autoreleasepool{
        char init;
        int steps;
        State states[MAX_STATES]={0};
        parse("input.txt",&init,&steps,states);
        static int tape[TAPE_SIZE]={0};
        int pos=TAPE_MID;
        char state=init;
        for(int i=0;i<steps;i++){
            int val=tape[pos];
            Command c=states[state-'A'].cmds[val];
            tape[pos]=c.write;
            pos+=c.move;
            state=c.nextState;
        }
        int sum=0;
        for(int i=0;i<TAPE_SIZE;i++)sum+=tape[i];
        printf("%d\n",sum);
    }
    return 0;
}
