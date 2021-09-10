#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define  kpc2Mpc 1.E-3
#define  Mpc2kpc 1.E3
#define  kpc2kpc 1.
#define  Mpc2Mpc 1.

struct io_header_1
{
  int      npart[6];
  double   mass[6];
  double   time;
  double   redshift;
  int      flag_sfr;
  int      flag_feedback;
  int      npartTotal[6];
  int      flag_cooling;
  int      num_files;
  double   BoxSize;
  double   Omega0;
  double   OmegaLambda;
  double   HubbleParam; 
  char     fill[256- 6*4- 6*8- 2*8- 2*4- 6*4- 2*4 - 4*8];  /* fills to 256 Bytes */
} header1;



int     NumPart, Ngas;

struct particle_data 
{
  float  Pos[3];
  float  Vel[3];
  float  Mass;
  int    Type;
  
  float  Rho, U, Temp, Ne;
} *P;

int *Id;

double  Time, Redshift;

void read_ascii(char *file_name);
int write_gadget(char *fname);
int allocate_memory(void);


/* Here we load a snapshot file. It can be distributed
 * onto several files (for files>1).
 * The particles are brought back into the order
 * implied by their ID's.
 * A unit conversion routine is called to do unit
 * conversion, and to evaluate the gas temperature.
 */
int main(int argc, char **argv)
{
  
  // UNITS:
  //        input:  kpc/h   V_pec = a*x_dot
  //        output: kpc/h   Something = sqrt(a)*x_dot = V_pec/sqrt(a)
  //
  
  char file_in[100];
  char file_out[100];
  int num_lines;   // number of lines to ignore.

  double pmass;
  
  printf("Name of file to convert? \n");
  scanf("%s", file_in);
  printf("%s \n", file_in);
  printf("Name of output file? \n");
  scanf("%s", file_out);
  

  // Read ascii file
  //----------------
  read_ascii(file_in);
  
  //reordering();  /* call this routine only if your ID's are set properly */
  //unit_conversion();  /* optional stuff */
  
  write_gadget(file_out);

  return 0;
  
}

/* this template shows how one may convert from Gadget's units
 * to cgs units.
 * In this example, the temperate of the gas is computed.
 * (assuming that the electron density in units of the hydrogen density
 * was computed by the code. This is done if cooling is enabled.)
 */
int unit_conversion(void)
{
  double GRAVITY, BOLTZMANN, PROTONMASS;
  double UnitLength_in_cm, UnitMass_in_g, UnitVelocity_in_cm_per_s;
  double UnitTime_in_s, UnitDensity_in_cgs, UnitPressure_in_cgs, UnitEnergy_in_cgs;  
  double G, Xh, HubbleParam;
  
  int i;
  double MeanWeight, u, gamma;
  
  /* physical constants in cgs units */
  GRAVITY   = 6.672e-8;
  
  /* internal unit system of the code */
  UnitLength_in_cm= 3.085678e21;   /*  code length unit in cm/h */
  UnitMass_in_g= 1.989e43;         /*  code mass unit in g/h */
  UnitVelocity_in_cm_per_s= 1.0e5;
  
  UnitTime_in_s= UnitLength_in_cm / UnitVelocity_in_cm_per_s;
  UnitDensity_in_cgs= UnitMass_in_g/ pow(UnitLength_in_cm,3);
  UnitPressure_in_cgs= UnitMass_in_g/ UnitLength_in_cm/ pow(UnitTime_in_s,2);
  UnitEnergy_in_cgs= UnitMass_in_g * pow(UnitLength_in_cm,2) / pow(UnitTime_in_s,2);
  
  G=GRAVITY/ pow(UnitLength_in_cm,3) * UnitMass_in_g * pow(UnitTime_in_s,2);
  
  Xh= 0.76;  /* mass fraction of hydrogen */
  HubbleParam= 0.73;
  
  for(i=1; i<=NumPart; i++)
  {
    if(P[i].Type==0)  /* gas particle */
    {
      MeanWeight= 4.0/(3*Xh+1+4*Xh*P[i].Ne) * PROTONMASS;
      
	  /* convert internal energy to cgs units */
      
      u  = P[i].U * UnitEnergy_in_cgs/ UnitMass_in_g;
      
      gamma= 5.0/3;
      
	  /* get temperature in Kelvin */
      
      P[i].Temp= MeanWeight/BOLTZMANN * (gamma-1) * u;
    }
  }

  return 0;

}

//------------------------------------
// read_ascii
//------------------------------------
void read_ascii(char *file_name)
{
  FILE *fp;
  char dummyline[1001];
  char * small;
  int i,j;
  float a;


  if( (fp=fopen(file_name,"r")) == NULL)
  {
    printf("can't open file `%s`\n",file_name);
    exit(0);
  }
 
  //Read in Header info
  fscanf(fp, "%d %d %d %d %d %d  ",&(header1.npart[0]), &(header1.npart[1]),
	 &(header1.npart[2]), &(header1.npart[3]),
	 &(header1.npart[4]), &(header1.npart[5]) );

  fscanf(fp, "%lf %lf %lf %lf %lf %lf  ",&(header1.mass[0]), &(header1.mass[1]),
	 &(header1.mass[2]), &(header1.mass[3]),
	 &(header1.mass[4]), &(header1.mass[5]) );
  
  fscanf(fp, "%lf", &(header1.time));
  fscanf(fp, "%lf", &(header1.redshift));
  fscanf(fp, "%d", &(header1.flag_sfr));
  fscanf(fp, "%d", &(header1.flag_feedback));

  fscanf(fp, "%d %d %d %d %d %d  ",&(header1.npartTotal[0]), 
	 &(header1.npartTotal[1]),
	 &(header1.npartTotal[2]), &(header1.npartTotal[3]),
	 &(header1.npartTotal[4]), &(header1.npartTotal[5]) );

  fscanf(fp, "%d", &(header1.flag_cooling));
  fscanf(fp, "%d", &(header1.num_files));
  fscanf(fp, "%lf", &(header1.BoxSize));
  fscanf(fp, "%lf", &(header1.Omega0));
  fscanf(fp, "%lf", &(header1.OmegaLambda));
  fscanf(fp, "%lf", &(header1.HubbleParam));

  printf("end of header %lf %lf %d \n",
	 header1.OmegaLambda,header1.HubbleParam,header1.num_files);

  a = 1.0/(1.0+header1.redshift);
  NumPart=header1.npart[0]+header1.npart[1]+
    header1.npart[2]+header1.npart[3]+
    header1.npart[4]+header1.npart[5];
  //  NumPart=10;

  allocate_memory();

  fgets(dummyline,1000,fp);
  for(i=1; i<=NumPart; i++)
  {
    /*      fscanf(fp, "%d %d %f %f %f %f %f %f %f %f %f %f %s", 
	   &(Id[i]),&(P[i].Type),
	   &(P[i].Pos[0]), &(P[i].Pos[1]), &(P[i].Pos[2]), 
	   &(P[i].Vel[0]), &(P[i].Vel[1]), &(P[i].Vel[2]), 
	   &(P[i].U),&(P[i].Temp),&(P[i].Rho),&(P[i].Ne)
	   ,dummyline);*/
        fgets(dummyline,1000,fp);
    //        printf("hmmm %d %s ",i,dummyline);

	   small=strtok(dummyline," ");
    j=0;
    while(small !=NULL){
      if(j==0) Id[i]=atoi(small);
      else if(j==1) P[i].Type=atoi(small);
      else if(j==2)P[i].Mass=atof(small);
      else if(j==3)P[i].Pos[0]=atof(small);
      else if(j==4)P[i].Pos[1]=atof(small);
      else if(j==5)P[i].Pos[2]=atof(small);
      else if(j==6)P[i].Vel[0]=atof(small);
      else if(j==7)P[i].Vel[1]=atof(small);
      else if(j==8)P[i].Vel[2]=atof(small);
      else if(j==9)P[i].U=atof(small);
      else if(j==10)P[i].Temp=atof(small);
      else if(j==11)P[i].Rho=atof(small);
      else if(j==12)P[i].Ne=atof(small);
      j=j+1;
      small = strtok (NULL, " ");
      }
    // Converts kpc/h to kpc/h.
    P[i].Pos[0] *= kpc2kpc;
    P[i].Pos[1] *= kpc2kpc;
    P[i].Pos[2] *= kpc2kpc;
    // Convert V_pec to V_pec/sqrt(a)
    P[i].Vel[0] /= sqrt(a);
    P[i].Vel[1] /= sqrt(a);
    P[i].Vel[2] /= sqrt(a);
//    P[i].Ne = 0.0;
    if(i==1 || i==NumPart)
           printf("%d %d %f %f %f %f %f %f %f %f %f %f %f \n",
		  Id[i],P[i].Type,P[i].Mass,
	   P[i].Pos[0], P[i].Pos[1], P[i].Pos[2], 
	   P[i].Vel[0], P[i].Vel[1], P[i].Vel[2], 
	   P[i].U,P[i].Temp,P[i].Rho,P[i].Ne);
      }
  
}


//---------------------------------
// write_gadget
//---------------------------------
int write_gadget(char *fname)
{
  
  // Writes in gadget format.
  // Only one file.
  
  FILE *fd;
  int    k,dummy;
  int    n,pc_new;
  
#define SKIP fwrite(&dummy, sizeof(dummy), 1, fd);
  
  if(!(fd=fopen(fname,"w")))
  {
    printf("can't open file `%s`\n",fname);
    exit(0);
  }
  
  printf("writing `%s' ...\n",fname); fflush(stdout);
  
  dummy = 256;
  fwrite(&dummy, sizeof(dummy), 1, fd);
  fwrite(&header1, sizeof(header1), 1, fd);
  fwrite(&dummy, sizeof(dummy), 1, fd);
  
  dummy = 4 * NumPart;
  SKIP;
  for(k=0,pc_new=1;k<6;k++)
  {
    printf("writing pos %d %d \n", k,header1.npart[k]);
    for(n=0;n<header1.npart[k];n++)
    {
      fwrite(&P[pc_new].Pos[0], sizeof(float), 3, fd);
      pc_new++;
    }
  }
  SKIP;
  
  SKIP;
  for(k=0,pc_new=1;k<6;k++)
  {
    for(n=0;n<header1.npart[k];n++)
    {
      fwrite(&P[pc_new].Vel[0], sizeof(float), 3, fd);
      pc_new++;
    }
  }
  SKIP;
  
  SKIP;
  printf("dummy = %d\n", dummy);
  for(k=0,pc_new=1;k<6;k++)
  {
    printf("writing ID %d %d \n", k,header1.npart[k]);
    for(n=0;n<header1.npart[k];n++)
    {
      Id[pc_new] = pc_new;
      fwrite(&Id[pc_new], sizeof(int), 1, fd);
      pc_new++;
    }
  }
  SKIP;
  printf("dummy = %d\n", dummy);

  SKIP;
  for(k=0,pc_new=1;k<6;k++)
  {
    printf("writing Masses %d %d \n", k,header1.npart[k]);
    if(header1.mass[k]==0){
      for(n=0;n<header1.npart[k];n++)
	{
	  fwrite(&P[pc_new].Mass, sizeof(float), 1, fd);
	  pc_new++;
	}
    }
  }
  SKIP;
  printf("mass dummy = %d \n", dummy);
  
  //  dummy = NumPart;
  SKIP;
  if(header1.npart[0]>0)
  {
    //    printf("mass dummy = %d \n", header1.npart[0]);
    
    printf("gas dummy = %d \n", dummy);
    pc_new=1;
    k=0;
    for(n=0;n<header1.npart[k];n++)
      {
	if(n <3) printf("checking = %d %d %e \n", n,pc_new,P[pc_new].U);
	fwrite(&P[pc_new].U, sizeof(float), 1, fd);
	pc_new++;
      }
  }
  printf("dummy = %d\n", dummy);
  SKIP;
  printf("dummy = %d\n", dummy);
  
  fclose(fd);
  
  Time= header1.time;
  Redshift= header1.time;

  return 0;

}




/* this routine allocates the memory for the 
 * particle data.
 */
int allocate_memory(void)
{
  printf("allocating memory...\n");
  
  printf("numpart = %d\n", NumPart);
  
  if(!(P=malloc(NumPart*sizeof(struct particle_data))))
  {
    fprintf(stderr,"failed to allocate memory.\n");
    exit(0);
  }
  
  P--;   /* start with offset 1 */
  
  
  if(!(Id=malloc(NumPart*sizeof(int))))
  {
    fprintf(stderr,"failed to allocate memory.\n");
    exit(0);
  }
  
  Id--;   /* start with offset 1 */
  
  printf("allocating memory...done\n");

  return 0;

}




/* This routine brings the particles back into
 * the order of their ID's.
 * NOTE: The routine only works if the ID's cover
 * the range from 1 to NumPart !
 * In other cases, one has to use more general
 * sorting routines.
 */
int reordering(void)
{
  int i;
  int idsource, idsave, dest;
  struct particle_data psave, psource;
  
  
  printf("reordering....\n");
  
  for(i=1; i<=NumPart; i++)
  {
    if(Id[i] != i)
    {
      psource= P[i];
      idsource=Id[i];
      dest=Id[i];
      
      do
      {
        psave= P[dest];
        idsave=Id[dest];
        
        P[dest]= psource;
        Id[dest]= idsource;
        
        if(dest == i) 
          break;
        
        psource= psave;
        idsource=idsave;
        
        dest=idsource;
      }
      while(1);
    }
  }
  
  printf("done.\n");
  
  Id++;   
  free(Id);
  
  printf("space for particle ID freed\n");

  return 0;

}


















