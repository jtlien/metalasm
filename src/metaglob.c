#include "stdio.h"
#include "string.h"
#include "stdlib.h"
#include "math.h"

#define TRUE 1
#define FALSE 0
#define MAX_GLOBALS 10000
#define nullchar 0

//
// This program reads in all the .glb files for a link
//          checks to see if they are all uniq,
//            Then puts them out in Symbtable format for the haskell build
//               of the "linked" version of the meta-assembler
//            This version has all the GLOBAL labels in it from all
//             the sub assembles.
//
int noteof;
int debug;
int i;
int global_cnt;
char this_symbol[120];
char thisline[120];
int findex;
int charindex;
int this_value;
char valuestr[120];
char tempstr[200];

struct globalstuff
{
  char label[120];
  int  addrval;
} global_list[MAX_GLOBALS];

struct linebufferstuff
{
  char oneline[500];
} inbuffer[500];

FILE *thisfile,*outfile;

char  globalfilestr[300];

void mgetline( char *lineread, FILE *infile)
{
  char *eof;
  eof=fgets(lineread,120,infile);
  if (eof != NULL)
    {
      noteof=TRUE;
    }
  else
    {
      noteof=FALSE;
    }
}

//
void  skipblanks()
{
  while((thisline[charindex]==' ')&&(thisline[charindex]!='\n'))
    {
      charindex +=1;
    }

} // skipblanks

// read past directive until a ';' is reached or 500 lines or endoffile

int not_in_symbol_table( char *insymbol)
{
  int flag;
  int jj;

  if (debug ) { printf("In not_in_symbol_table \n"); }

  flag=FALSE;
  jj=0;
  while( ( jj < global_cnt) && ( flag ==FALSE))
    {
      if (strcmp(global_list[jj].label, insymbol)==0)
	{
          flag=TRUE;
        }
      jj+=1;
    }

  if (debug) { printf("Returning from not_in_symbol_table \n"); }

  return(! flag);

 }  // not_in_symbol_table

void get_value()
{


  int kk;
  kk=0;
  valuestr[0]=nullchar;

  while((kk<120)&&(thisline[charindex]!=' ') &&(thisline[charindex]!='\n'))
    {
      valuestr[kk]= thisline[charindex];
      kk+=1;
      charindex+=1;
    }

  valuestr[kk]= nullchar;

  if (debug) {  printf("valuestr = %s \n",valuestr); }

  // this_value=atoi(valuestr);

  strncpy(tempstr,"0x",5);
  strncat(tempstr,valuestr,100);
 
  if (debug) { printf("tempstr = %s \n",tempstr); }

  sscanf(tempstr,"%x",&this_value);
  


} // get_value


void get_symbol()
{
  int kk;
  kk=0;
  charindex=0;
  this_symbol[0]=nullchar;

  while((kk<120)&&(thisline[charindex]!=' ') &&(thisline[charindex]!='\n'))
    {
      this_symbol[kk]= thisline[charindex];
      kk+=1;
      charindex+=1;
    }

  this_symbol[kk]= nullchar;

} // get_symbol


void add_to_sym_table(char *insymbol, int symvalue)
{

  if ((global_cnt+1) < MAX_GLOBALS)
    {
      strncpy(global_list[global_cnt].label,insymbol,120);
      global_list[global_cnt].addrval=symvalue;
    }
  global_cnt += 1;
}


int main(int argc, char *argv[])
{
  debug = 0;

  global_cnt=0;

  noteof=TRUE;
  
  findex = 1;

  while( findex < argc)
    {
      if (debug) { printf("About to open file = %s \n",argv[findex]); }

      thisfile = fopen(argv[findex],"r");

      if (thisfile != NULL)
       {
         noteof=TRUE;
         mgetline(thisline,thisfile);
	 while(noteof)
           {
            
            charindex=0;
            get_symbol(thisline);
            if (debug) { printf("Got the symbol = %s \n",this_symbol); }

            if (not_in_symbol_table( this_symbol) )
              {

                skipblanks();
                get_value(thisline);
                if (debug) { printf("Got value= %d \n",this_value); }

		add_to_sym_table(this_symbol,this_value);
              }
            else
              {

                skipblanks();
                get_value(thisline);

		printf("Duplicate symblol: %s in file = %s \n",
		       this_symbol, argv[findex]);
              }

            mgetline(thisline,thisfile);
            if (debug) { printf("Got another line = %s \n",thisline); }

	   }
         fclose(thisfile);
         noteof =TRUE;
         findex += 1;
       }
      else
	{
          printf("Unable to open the global file = %s \n",argv[findex]);
          findex +=1;
	}

    }
   


  outfile = fopen("globals","w");

  if (outfile == NULL)
    {
      printf("Unable to open the output file %s \n","globals");

    }

  fprintf(outfile,"globals=[ ");

  
  for(i=0; i < global_cnt-1; i+= 1)
    {
      if (i % 6 == 5)
	{
      fprintf(outfile,"(%d, %s),\n",global_list[i].addrval, global_list[i].label);
         if (i!= 0)
	  {
          fprintf(outfile,"          ");
	  }
        }
      else
	{
      fprintf(outfile,"(%d, %s),",global_list[i].addrval, global_list[i].label);
	}
    }

   fprintf(outfile,"(%d, %s)]\n",global_list[i].addrval, global_list[i].label);
  fclose(outfile);
  
 }  // end main
