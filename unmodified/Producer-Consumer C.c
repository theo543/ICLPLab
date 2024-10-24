// Avem o ierarhie tata-fiu
// si vrem ca cele doua procese sa comunice prin intermediul unui segment partajat de memorie 
// segmentul are dimensiunea de 1B 
// tatal scrie in acel segment caracterele de la 'a' la 'z' 
// fiul trebuie sa le consume pe masura ce tatal completeaza informatii in respectiva zona 

// este o varianta de Producer-Consumer in IPC 

// LABORATORUL DE ICLP INCEPE IN FIECARE VINERI LA 8:30 VA ROG NU 
// CU ACCENT PE NU
// NU VENITI MAI REPEDE 
// N U  VENITI MAI RAPID 
// N U  MA GASITI MAI RAPID 
// MULTUMESC! 

#include <stdio.h>
#include <unistd.h>
#include <sys/sem.h> 
#include <sys/shm.h>
#include <sys/wait.h> 
#include <sys/stat.h> 

int main(int argc, char* argv[]) 
{
    int semprnt = semget(IPC_PRIVATE, 1, S_IRUSR | S_IWUSR);
    int semchld = semget(IPC_PRIVATE, 1, S_IRUSR | S_IWUSR);
    
    // declar obiectul IPC_PRIVATE -> este vizibil in ierarhia curenta 
    // 1 - dimensiunea (in Bytes) a segmentului partajat de memorie 
    // drepturile pe care le dau asupra zonei 
    int shmid = shmget(IPC_PRIVATE, 1, S_IRUSR | S_IWUSR);
    
    semctl(semprnt, 0, SETVAL, 0);
    semctl(semchld, 0, SETVAL, 0);
    
    int childPid; 
    if ((childPid = fork()) > 0) 
    {
        // parent 
        // printf("Child pid: %d\n", childPid);
        
        char *p = shmat(shmid, NULL, SHM_RND);
        
        struct sembuf releaseFather;
        struct sembuf acquireChild; 
        
        releaseFather.sem_num = 0;
        releaseFather.sem_op = 1; 
        releaseFather.sem_flg = 0; 
        
        acquireChild.sem_num = 0;
        acquireChild.sem_op = -1;
        acquireChild.sem_flg = 0;
        
        for (char ch = 'a'; ch <= 'z'; ch++) 
        {
            *p = ch; 
            semop(semprnt, &releaseFather, 1);
            semop(semchld, &acquireChild, 1);
        }
        
        // parintele trebuie sa isi astepte toti copiii 
        wait(NULL);
        
        // obligatoriu eliberam aceste resurse in tata 
        // dupa ce si-a sincronizat toti fiii 
        // altfel raman obiecte in tabela de IPC in sistemul de operare 
        // si vor exista acolo pana la finalul de viata al instantei
        
        semctl(semprnt, 0, IPC_RMID, NULL);
        semctl(semchld, 0, IPC_RMID, NULL);
        
        // nu uitam sa detasam zona de memorie 
        shmdt(p);
    }
    else 
    {
        // child 
        // printf("Parent pid: %d\n", getppid());
        char *p = shmat(shmid, NULL, SHM_RND);
        
        struct sembuf acquireFather;
        struct sembuf releaseChild;
        
        acquireFather.sem_num = 0;
        acquireFather.sem_op = -1;
        acquireFather.sem_flg = 0;
        
        releaseChild.sem_num = 0;
        releaseChild.sem_op = 1;
        releaseChild.sem_flg = 0;
        
        for (int i = 0; i < 26; i++)
        {
            semop(semprnt, &acquireFather, 1);
            
            printf("%c", *p);
            fflush(NULL);
            
            semop(semchld, &releaseChild, 1);
        }
    }
}