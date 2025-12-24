
#import <Foundation/Foundation.h>
#import <stdio.h>
#import <stdlib.h>
#import <string.h>
#import <stdbool.h>
#import <math.h>

#define MAX_TILES 144
#define MAX_TILE_SIZE 10
#define MAX_LINE_LEN 128
#define MAX_TILES_SQRT 12
#define MAX_IMAGE_DIM (MAX_TILES_SQRT * (MAX_TILE_SIZE - 2))
#define MAX_MONSTER_PARTS 100

typedef struct{int r,c;} Coord;
typedef struct{
    int id;
    char contents[MAX_TILE_SIZE][MAX_TILE_SIZE];
    int size;
    char orientations[8][MAX_TILE_SIZE][MAX_TILE_SIZE];
} Tile;
typedef struct{
    int tile_index;
    int orientation_index;
} AssembledTileInfo;

Tile tiles[MAX_TILES];
int num_tiles=0;
int tile_size=0;
int grid_edge_size=0;
AssembledTileInfo assembled_info[MAX_TILES_SQRT][MAX_TILES_SQRT];
bool used_indices[MAX_TILES];
char image[MAX_IMAGE_DIM][MAX_IMAGE_DIM];
int image_dim=0;
Coord monster_offsets[MAX_MONSTER_PARTS];
int num_monster_offsets=0;
int monster_height=0;
int monster_width=0;

void copy_grid(char d[MAX_TILE_SIZE][MAX_TILE_SIZE], const char s[MAX_TILE_SIZE][MAX_TILE_SIZE], int sz){for(int i=0;i<sz;i++)memcpy(d[i],s[i],sz);}
void rotate_grid(char d[MAX_TILE_SIZE][MAX_TILE_SIZE], const char s[MAX_TILE_SIZE][MAX_TILE_SIZE], int sz){for(int i=0;i<sz;i++)for(int j=0;j<sz;j++)d[j][sz-1-i]=s[i][j];}
void mirror_grid(char d[MAX_TILE_SIZE][MAX_TILE_SIZE], const char s[MAX_TILE_SIZE][MAX_TILE_SIZE], int sz){for(int i=0;i<sz;i++)for(int j=0;j<sz;j++)d[i][sz-1-j]=s[i][j];}
void generate_orientations(Tile *t){
    char tmp[MAX_TILE_SIZE][MAX_TILE_SIZE];
    copy_grid(t->orientations[0],t->contents,t->size);
    copy_grid(tmp,t->orientations[0],t->size);
    for(int i=1;i<4;i++){rotate_grid(t->orientations[i],tmp,t->size);copy_grid(tmp,t->orientations[i],t->size);}
    mirror_grid(t->orientations[4],t->orientations[0],t->size);
    copy_grid(tmp,t->orientations[4],t->size);
    for(int i=5;i<8;i++){rotate_grid(t->orientations[i],tmp,t->size);copy_grid(tmp,t->orientations[i],t->size);}
}
bool compare_top_bottom(const char a[MAX_TILE_SIZE][MAX_TILE_SIZE], const char b[MAX_TILE_SIZE][MAX_TILE_SIZE], int sz){
    for(int c=0;c<sz;c++)if(a[sz-1][c]!=b[0][c])return false;return true;
}
bool compare_left_right(const char a[MAX_TILE_SIZE][MAX_TILE_SIZE], const char b[MAX_TILE_SIZE][MAX_TILE_SIZE], int sz){
    for(int r=0;r<sz;r++)if(a[r][sz-1]!=b[r][0])return false;return true;
}
bool backtrack_assemble(int r,int c){
    if(r==grid_edge_size)return true;
    int nr=r,nc=c+1;if(nc==grid_edge_size){nr=r+1;nc=0;}
    for(int i=0;i<num_tiles;i++)if(!used_indices[i]){
        for(int o=0;o<8;o++){
            const char (*cur)[MAX_TILE_SIZE]=tiles[i].orientations[o];
            if(r>0){
                int ai=assembled_info[r-1][c].tile_index;
                int ao=assembled_info[r-1][c].orientation_index;
                if(!compare_top_bottom(tiles[ai].orientations[ao],cur,tile_size))continue;
            }
            if(c>0){
                int li=assembled_info[r][c-1].tile_index;
                int lo=assembled_info[r][c-1].orientation_index;
                if(!compare_left_right(tiles[li].orientations[lo],cur,tile_size))continue;
            }
            assembled_info[r][c].tile_index=i;
            assembled_info[r][c].orientation_index=o;
            used_indices[i]=true;
            if(backtrack_assemble(nr,nc))return true;
            used_indices[i]=false;
        }
    }
    return false;
}
void assemble_image(){
    int sub=tile_size-2;
    image_dim=grid_edge_size*sub;
    for(int br=0;br<grid_edge_size;br++)for(int bc=0;bc<grid_edge_size;bc++){
        int ti=assembled_info[br][bc].tile_index;
        int oi=assembled_info[br][bc].orientation_index;
        const char (*tg)[MAX_TILE_SIZE]=tiles[ti].orientations[oi];
        for(int r=0;r<sub;r++)for(int c=0;c<sub;c++)image[br*sub+r][bc*sub+c]=tg[r+1][c+1];
    }
}
void setup_monster(){
    const char *p[]={"                  # ","#    ##    ##    ###"," #  #  #  #  #  #   "};
    monster_height=3;
    monster_width=strlen(p[0]);
    num_monster_offsets=0;
    for(int r=0;r<monster_height;r++)for(int c=0;c<monster_width;c++)if(p[r][c]=='#'){
        monster_offsets[num_monster_offsets++] = (Coord){r,c};
    }
}
void rotate_image_grid(char d[MAX_IMAGE_DIM][MAX_IMAGE_DIM], const char s[MAX_IMAGE_DIM][MAX_IMAGE_DIM], int dim){
    for(int i=0;i<dim;i++)for(int j=0;j<dim;j++)d[j][dim-1-i]=s[i][j];
}
void mirror_image_grid(char d[MAX_IMAGE_DIM][MAX_IMAGE_DIM], const char s[MAX_IMAGE_DIM][MAX_IMAGE_DIM], int dim){
    for(int i=0;i<dim;i++)for(int j=0;j<dim;j++)d[i][dim-1-j]=s[i][j];
}
int find_and_mark_monsters(char cur[MAX_IMAGE_DIM][MAX_IMAGE_DIM]){
    bool found=false;
    static bool part[MAX_IMAGE_DIM][MAX_IMAGE_DIM];
    memset(part,0,sizeof(part));
    for(int r=0;r<=image_dim-monster_height;r++)for(int c=0;c<=image_dim-monster_width;c++){
        bool ok=true;
        for(int i=0;i<num_monster_offsets;i++){
            int mr=r+monster_offsets[i].r, mc=c+monster_offsets[i].c;
            if(cur[mr][mc]!='#'){ok=false;break;}
        }
        if(ok){
            found=true;
            for(int i=0;i<num_monster_offsets;i++){
                int mr=r+monster_offsets[i].r, mc=c+monster_offsets[i].c;
                part[mr][mc]=true;
            }
        }
    }
    if(!found)return -1;
    int cnt=0;
    for(int r=0;r<image_dim;r++)for(int c=0;c<image_dim;c++)if(cur[r][c]=='#' && !part[r][c])cnt++;
    return cnt;
}
void parse_input(FILE *fp){
    char line[MAX_LINE_LEN];
    int curRow=0;
    while(fgets(line,sizeof(line),fp)){
        if(strncmp(line,"Tile ",5)==0){
            if(num_tiles>0 && curRow!=tiles[num_tiles-1].size){
                fprintf(stderr,"Error: Inconsistent tile size at tile %d\n",tiles[num_tiles-1].id);
                exit(EXIT_FAILURE);
            }
            sscanf(line,"Tile %d:",&tiles[num_tiles].id);
            curRow=0;
        }else if(line[0]=='\n' || line[0]=='\r'){
            if(curRow>0){
                tiles[num_tiles].size=curRow;
                if(tile_size==0)tile_size=curRow;
                else if(tile_size!=curRow){fprintf(stderr,"Error: Inconsistent tile sizes (%d vs %d)\n",tile_size,curRow);exit(EXIT_FAILURE);}
                generate_orientations(&tiles[num_tiles]);
                num_tiles++;
                if(num_tiles>=MAX_TILES){fprintf(stderr,"Error: Exceeded MAX_TILES\n");exit(EXIT_FAILURE);}
            }
        }else{
            int len=strlen(line);
            while(len>0 && (line[len-1]=='\n' || line[len-1]=='\r'))line[--len]='\0';
            if(curRow<MAX_TILE_SIZE && len>0){
                if(curRow==0 && tile_size==0){tile_size=len;if(tile_size>MAX_TILE_SIZE){fprintf(stderr,"Error: Tile size %d exceeds MAX_TILE_SIZE %d\n",tile_size,MAX_TILE_SIZE);exit(EXIT_FAILURE);}}
                else if(len!=tile_size){fprintf(stderr,"Error: Inconsistent row length in tile %d (expected %d, got %d)\n",tiles[num_tiles].id,tile_size,len);exit(EXIT_FAILURE);}
                memcpy(tiles[num_tiles].contents[curRow],line,tile_size);
                curRow++;
            }
        }
    }
    if(curRow>0){
        tiles[num_tiles].size=curRow;
        if(tile_size==0)tile_size=curRow;
        else if(tile_size!=curRow){fprintf(stderr,"Error: Inconsistent tile sizes (%d vs %d) for last tile\n",tile_size,curRow);exit(EXIT_FAILURE);}
        generate_orientations(&tiles[num_tiles]);
        num_tiles++;
    }
    double s=sqrt((double)num_tiles);
    if(s!=floor(s) || num_tiles==0){fprintf(stderr,"Error: Number of tiles (%d) is not a perfect square or is zero.\n",num_tiles);exit(EXIT_FAILURE);}
    grid_edge_size=(int)s;
    if(grid_edge_size>MAX_TILES_SQRT){fprintf(stderr,"Error: Grid edge size %d exceeds MAX_TILES_SQRT %d\n",grid_edge_size,MAX_TILES_SQRT);exit(EXIT_FAILURE);}
    int expImg=grid_edge_size*(tile_size-2);
    if(expImg>MAX_IMAGE_DIM){fprintf(stderr,"Error: Calculated image dimension %d exceeds MAX_IMAGE_DIM %d\n",expImg,MAX_IMAGE_DIM);exit(EXIT_FAILURE);}
}
int main(int argc,char *argv[]){
    FILE *fp=fopen("input.txt","r");
    if(!fp){perror("Error opening input.txt");return EXIT_FAILURE;}
    parse_input(fp);
    fclose(fp);
    if(tile_size==0){fprintf(stderr,"Error: No tiles parsed or tile size is zero.\n");return EXIT_FAILURE;}
    setup_monster();
    memset(used_indices,0,sizeof(used_indices));
    if(!backtrack_assemble(0,0)){fprintf(stderr,"Error: Failed to assemble tiles.\n");return EXIT_FAILURE;}
    assemble_image();
    char cur[MAX_IMAGE_DIM][MAX_IMAGE_DIM];
    char tmp[MAX_IMAGE_DIM][MAX_IMAGE_DIM];
    for(int i=0;i<image_dim;i++)memcpy(cur[i],image[i],image_dim);
    int final=-1;
    for(int m=0;m<2;m++){
        for(int r=0;r<4;r++){
            int rough=find_and_mark_monsters(cur);
            if(rough!=-1){final=rough;goto done;}
            rotate_image_grid(tmp,cur,image_dim);
            for(int i=0;i<image_dim;i++)memcpy(cur[i],tmp[i],image_dim);
        }
        mirror_image_grid(tmp,image,image_dim);
        for(int i=0;i<image_dim;i++)memcpy(cur[i],tmp[i],image_dim);
        for(int i=0;i<image_dim;i++)memcpy(image[i],tmp[i],image_dim);
    }
done:
    if(final==-1){fprintf(stderr,"Error: Sea monsters not found in any orientation.\n");return EXIT_FAILURE;}
    printf("%d\n",final);
    return EXIT_SUCCESS;
}
