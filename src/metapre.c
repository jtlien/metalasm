
#include "stdio.h"
#include "string.h"
#include "stdlib.h"
#include "math.h"

#define TRUE 1
#define FALSE 0
#define MAX_GLOBALS 10000

// This preprocessor will read the meta-assembler source and
//        handle the directives
//           *ORG  value    --  set the current address to value
//           *REPEAT value   --  repeat the next instruction n times (value=n)
//           *SKIP   value   -- increment the current address by n (value=n)
//           *GLOBAL label  -- declare a global label
//           *ALIGN   value  --  value must be power of 2.  Set the
//                                current address to the next larger address
//                                such that address mod n = 0 where (value=n)
//                                 
//           *INCLUDE value -- include the file name=value
//
int noteof;
char directivestr[120];
char directivevalstr[120];
int dvalue;
char thisline[320];
int charindex;
char nullchar=0;
int currentadd;
int global_cnt;
int gg;
int debug;

struct globalstuff
{
  char label[120];
  int  addrval;
} global_list[MAX_GLOBALS];

struct linebufferstuff
{
  char oneline[500];
} inbuffer[500];

FILE *thisfile,*outfile,*outfile1;
int buflines;
int directivegood;
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

int linehassemi( char *instr)
{
  char *tptr;
  tptr=instr;
  while((*tptr != ';')&&(*tptr!=nullchar))
    {
      tptr++;
    }
  if (*tptr == ';')
    {
      return(TRUE);
    }
  return(FALSE);
}

// read past directive until a ':' is reached 

int linehascolon( char *instr)
{
  char *tptr;
  tptr=instr;
  while((*tptr != ':')&&(*tptr!=nullchar))
    {
      tptr++;
    }
  if (*tptr == ':')
    {
      return(TRUE);
    }
  return(FALSE);
}

void handle_label( char *inlabel)
{
  int gindex;
  int match;

  gindex=0;
  match=FALSE;

  while((gindex < global_cnt) && (match == FALSE))
    {
      if (strcmp(global_list[gindex].label, inlabel) == 0 )
	{
	  match = TRUE;
          global_list[gindex].addrval = currentadd;
        }
      else
	{
          gindex += 1;
        }
    }

}
void read_statements_tobuffer()
{
  int linecnt;
  int ii;
  char labelstr[120];

  linecnt=0;
  while((linecnt<500)&&(linehassemi(thisline) == FALSE)&&(noteof))
    {
      if (strlen(thisline) < 500)
       {
	 strncpy(inbuffer[linecnt].oneline,thisline,320);
         if (linehascolon(thisline)==TRUE)
	   {
             strncpy(labelstr,thisline,120);
             ii=0;
             while((labelstr[ii] != ':') && (ii<120))
	       {
		 ii+=1;
               }
	     if ((labelstr[ii]==':') && ( ii < 120))
	       {
		 labelstr[ii] = nullchar;
	        handle_label(labelstr);
               }
           }
       }
      mgetline(thisline,thisfile);
      linecnt+=1;
      if (debug) 
	{ printf("Got line in read_statements_tobuffer= %s \n",thisline); }
     }

  strncpy(inbuffer[linecnt].oneline,thisline,320);

  buflines=linecnt+1;

} // read_statements_tobuffer

void write_statements_frombuffer()
{
  int linecnt;
 
  linecnt=0;
  while(linecnt< buflines)
    {
      fprintf(outfile,"%s",inbuffer[linecnt].oneline);
 
      linecnt+= 1;
     }
 

} // write_statements_frombuffer

int convert_str_toint( char *instr)
{
  int retval;
  char *tptr;
  int ii;
    int powof2;
  int intval;
  int powint;
  
  int temp;
  temp=0;
  if (debug)
    {
     printf("In convert_str_toint - instr = %s \n",instr);
    }

  if ((instr[1] == 'x') || (instr[1] == 'b') || (instr[1] == 'o'))
    {
      if (strlen(instr) > 2)
       {
        if ((instr[0] == '0') && (instr[1] == 'b'))
        {
	  // retval=sscanf(instr,"%b",&temp);
	  powof2 = strlen(instr) - 3;
	  ii=2;
	  intval = 0;
	  
	  while (ii < strlen(instr))
	    {
	      if (instr[ii] == '1')
		{
		  powint = 2^powof2;
		  intval = intval + powint;
		}
	      
	       powof2 -= 1;
	       ii +=1;
	       
	    }
	  retval = intval;
        }
        if ((instr[0] == '0') && (instr[1] == 'o'))
        {
	  retval=sscanf(instr,"%o",&temp);
        }
        if ((instr[0] == '0') && (instr[1] == 'x'))
        {
         
	  retval=sscanf(instr,"%x",&temp);
        }
       }
    }
   else
      {
	//retval = sscanf(instr,"%d",temp);
        temp = atoi(instr);
      }
  return(temp);

}  // convert_str_toint


void do_org()
{
  if (debug)
    {
     printf("In do org \n");
    }

  dvalue=convert_str_toint( directivevalstr);

  if (debug)
    {
     printf("Dvalue in do_org = %d \n", dvalue);
    }

  if (dvalue > 0 ) 
    {
      currentadd= dvalue;
    }
  else
    {
      if (dvalue < 0 )
	{
         printf("ORG directive has negative address \n");
         printf("   Line = %s\n", thisline);
	}
      else
	{
	  currentadd=0;
        }
    }
}  // do_org

void do_repeat()
{
  mgetline(thisline,thisfile); // prime the pump

  dvalue=convert_str_toint( directivevalstr);

  if (debug) { printf("In do repeat - dvalue = %d \n",dvalue); }


  if (dvalue > 0 ) 
    {
      read_statements_tobuffer();

      while(dvalue > 0 )
	{
          fprintf(outfile,"ORG=#%X;\n",currentadd);

          write_statements_frombuffer();                  

          currentadd += 1;

          dvalue=dvalue-1;
        }
    }
  else
    {
      if (dvalue < 0 )
	{
         printf("REPEAT directive has negative repeat value \n");
         printf("   Line = %s\n", thisline);
	}
      else
	{
	  printf("REPEAT directive has zero repeat value \n");
        }
    }
}  // do_repeat

void do_global()
{

  char thisglobal[200];
  
  strncpy( thisglobal,directivevalstr,120);

  if (debug) { printf("In do global - global label  = %s \n",thisglobal); }

  if (global_cnt < MAX_GLOBALS)
    {
      strncpy( global_list[global_cnt].label, thisglobal, 120);
      global_cnt += 1;
      global_list[global_cnt].addrval = -1;
    }
  
}  // do_global


void do_skip()
{

  dvalue=convert_str_toint( directivevalstr);

  if (dvalue > 0 ) 
    {
      currentadd= currentadd + dvalue;
    }
  else
    {
      if (dvalue < 0 )
	{
         printf("SKIP directive has negative value \n");
         printf("   Line = %s\n", thisline);
	}
      else  // in case of 0, nothing
	{
        }
    }
}  // do_skip

int ispowerof2( int inval)
{
  int jj;
  int temp;
  temp=2;

  jj=0;
  while(jj<32)
    {
      if (temp == inval)
        {
          return(TRUE);
        }
      jj += 1;
      temp=temp+temp;
    }
  return(FALSE);
}  //
       

int isnotblank( char *instr)
{
  char *tstr;

  tstr=instr;
 
  while((*tstr == ' ') && (*tstr != '\n'))
    {
      tstr++;
    }
  if ((*tstr == '\n') || ( *tstr == '\r'))
    {
      return(FALSE);
    }
  return(TRUE);

}  // isnotblank (line)
       
void do_align()
{
  int exess;

  dvalue=convert_str_toint( directivevalstr);
  exess=0;

  if (dvalue > 0 ) 
    {
      if ( ispowerof2(dvalue))
        {
          if ( dvalue > 1)
            {
             exess=currentadd % dvalue;
             currentadd = currentadd + ( dvalue - exess);
            }
	}
      else
	{
          printf("ALIGN directive value is not a positive power of 2 \n");
          printf("   Input line = %s \n",thisline);
        }
    }
  else
    {
      if (dvalue == 0 )
        {
          printf("ALIGN directive value must not be zero \n");
          printf("   Line = %s\n", thisline);
        }
      else
        {
         printf("ALIGN directive has negative value \n");
         printf("   Line = %s\n", thisline);
	}
    }

}  // do_align

//
//  lines that start with * have (directive) (directivevalue) string
//          *ORG 235;   for example
//
void read_directive()
{
  int ii;
  int jj;

  charindex=1;
  ii=0;
  while((ii<120)&&(thisline[charindex]!=' ')&&(thisline[charindex]!='\n'))
    {
      directivestr[ii] = thisline[charindex];
      if (debug) { printf("directivestr ii = %d %c \n",ii,directivestr[ii]); }
      charindex+=1;
      ii += 1;
    }
  directivestr[ii]=nullchar;
 
  if (debug ) { printf("Directivestr = %s \n",directivestr); }

  if(thisline[charindex]==' ')
    {
      skipblanks();
      jj=0;
      directivevalstr[0]=nullchar;

     while((jj<120)&&(thisline[charindex]!=' ')&&(thisline[charindex]!='\n')
	   && (thisline[charindex] != '\r'))
      {
       directivevalstr[jj] = thisline[charindex];
       if (debug)
	 {
          printf("directivevalstr jj = %d %d \n",jj,directivevalstr[jj]);
         }
       charindex+=1;
       jj += 1;
      }
     directivevalstr[jj]=nullchar;
    }
    
  if (debug)
    {
  printf("Leaving read_directive with %s %s = dirstr valstr \n",directivestr,
	 directivevalstr);
    }

  // getline(thisline,thisfile);

} // read_directive

int main(int argc, char *argv[])
{
  debug = 0;

  global_cnt=0;

  noteof=TRUE;
  thisfile = fopen(argv[1],"r");
  if (thisfile == NULL)
    {
      printf("Unable to open the input file %s \n", argv[1]);
      exit(0);
    }
   
  if (thisfile != NULL)
    {
      mgetline(thisline,thisfile);
    }
  
   
  if (debug) { printf("First line=%s \n",thisline); }

  outfile = fopen(argv[2],"w");
  if (outfile == NULL)
    {
      printf("Unable to open the output file %s \n", argv[2]);

    }

  
  
  currentadd=0;  // the current address

  while(noteof)
    {
     
      if (thisline[0] == '*')
        {
     
	  directivestr[0]=nullchar;
        read_directive();
        if (strlen(directivestr) > 0 )
	  {
	    directivegood=FALSE;
           if (strcmp(directivestr,"ORG") == 0 )
	    {
             do_org();
             directivegood=TRUE;
	    }
           if (strcmp(directivestr,"ALIGN") == 0 )
	    {
             do_align();
             directivegood=TRUE;
	    }
           if (strcmp(directivestr,"SKIP") == 0 )
	   {
             do_skip();
             directivegood=TRUE;
	    }
           if (strcmp(directivestr,"REPEAT") == 0 )
	   {
             do_repeat();
             directivegood=TRUE;
	   }
           if (strcmp(directivestr,"INCLUDE") == 0 )
	    {
	      // do_include();
              directivegood=TRUE;
	    }
	   if (strcmp(directivestr,"GLOBAL") == 0 )
	    {
	      do_global();
              directivegood=TRUE;
	    }
	   if (directivegood==FALSE)
             {
	       printf("Unrecognized directive \n");
               printf(" Line in = %s \n",thisline);
	     }
	  }
	 } // line starts with *
	else
          {
            if (isnotblank(thisline))
	      {
               fprintf(outfile,"ORG=#%X;\n",currentadd);
               read_statements_tobuffer();
               write_statements_frombuffer();
               currentadd +=1;
              }
            
          }
         mgetline(thisline,thisfile);
         if (debug ) { printf("The new line = %s \n",thisline); }
       }

  fclose(outfile);

  strncpy(globalfilestr,argv[2],100);
  gg=0;
  while(gg < strlen(globalfilestr))
    {
      if (globalfilestr[gg] == '.')
	{
	  globalfilestr[gg] = nullchar;
        }
      gg+=1;
    }
  strncat( globalfilestr,".glb",30);

  gg=0;
  if (global_cnt > 0 )
    {
      outfile1=fopen(globalfilestr,"w");
      if (outfile1 == NULL)
	{
          printf("Unable to open the output file = %s \n", globalfilestr);
          
        }
     
     while( gg < global_cnt )
     {
      if (global_list[gg].addrval == -1)
	{
	  printf("Label was made global, but not found in source \n");
          printf(" For label = %s \n", global_list[gg].label);
	}
      else
	{
          fprintf(outfile1,"%s %x \n", global_list[gg].label,
		  global_list[gg].addrval);
        }
      gg+=1;
     }
    fclose(outfile1);
    }
  
 }  // end main
