#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/sem.h> 
#include <sys/stat.h>

// obiectele din IPC System V sunt tinute in sistemul de operare 
// pe toata durata de viata a acestuia 
// semafoarele, cozile de mesaje, segmentele de memorie partajata 

// modus ponens. 

// fiecare obiect din ICP System V are o cheie si un identificator 
// seamana cu numele fisierului si descriptorul lui 

// putem accesa obiecte deja create, preluand o cheie deja existenta 
// sau putem crea un obiect nou 
// in modul PRIVATE 
// care va fi accesibil doar procesului parinte si copiilor sai 

// IPC_PRIVATE

int main(int argc, char* argv[]) 
{
    // cheie privata, dimensiunea semaforului este 1, cu drept de citire si de scriere pt user 
    int mutex = semget(IPC_PRIVATE, 1, S_IRUSR | S_IWUSR);
    
    // setam valoarea semaforului la 0 
    semctl(mutex, 0, SETVAL, 0);
    
    if (fork())
    {
        // father 
        
        // mesajele in semafor sunt date prin intermediul structurii sembuf 
        struct sembuf release; 
        release.sem_num = 0;
        release.sem_op = 1;
        release.sem_flg = 0;
        
        for (int i = 0; i < 255; i++) 
        {
            printf("%c", '1');
            fflush(NULL);
        }
        
        // pe ce obiect, ce operatie, cate operatii trimit 
        semop(mutex, &release, 1);
        
        wait(NULL);
        
        // stergem obiectul creat 
        // mereu apare in parinte 
        semctl(mutex, 0, IPC_RMID, NULL);
    }
    else 
    {
        // child 
        struct sembuf acquire; 
        acquire.sem_num = 0;
        acquire.sem_op = -1;
        acquire.sem_flg = 0;
        
        semop(mutex, &acquire, 1);
        
        for (int i = 0; i < 255; i++) 
        {
            printf("%c", '0');
            fflush(NULL);
        }
    }
}