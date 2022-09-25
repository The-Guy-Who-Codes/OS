#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef uint8_t bool;
#define true 1
#define false 0


typedef struct {

    // defines the variables which must be defined at the start of a device using the Fat12 file system

    uint8_t BootJumpInstruction[3];
    uint8_t OEMIdentifier[8];
    uint16_t BytesPerSector;
    uint8_t SectorsPerCluster;
    uint16_t ReservedSectors;
    uint8_t FatCount;
    uint16_t DirectoryEntryCount;
    uint16_t TotalSectors;
    uint8_t MediaDescriptorType;
    uint16_t SectorsPerFat;
    uint16_t SectorsPerTrack;
    uint16_t HeadsPerCylinder;
    uint32_t HiddenSectors;
    uint32_t LargeSectorCount;

    // Extended boot record

    uint8_t DriveNumber;
    uint8_t _Reserved;
    uint8_t Signature;
    uint32_t VolumeID;
    uint8_t VolumeLabel[11];
    uint8_t SystemID[8];



} __attribute__((packed)) BootSector; // __attribute__((packed)) tells the compiler not to add padding code to the struct


typedef struct {
    uint8_t FileName[11];
    uint8_t Attribute;
    uint8_t Reserved;
    uint8_t CreationTimeTenths;
    uint16_t TimeOfCreation;
    uint16_t DateOfCreation;
    uint16_t AccessedDate;
    uint16_t FirstClusterHigh;
    uint16_t ModificationTime;
    uint16_t ModificationDate;
    uint16_t FirstClusterLow;
    uint32_t FileSize;

} __attribute__((packed)) DirectoryEntry;

BootSector g_BootSector;
uint8_t* g_Fat;
DirectoryEntry* g_RootDirectory;
uint8_t g_RootDirectoryEnd;

bool readBootSector(FILE* disk) {
    return fread(&g_BootSector, sizeof(g_BootSector), 1, disk);

}


bool readSectors(FILE* disk, uint32_t lba, uint32_t count, void* bufferOut) { // lba is the sector number from where the reading begins
    bool ok = true;
    ok = ok && (fseek(disk, lba * g_BootSector.BytesPerSector, SEEK_SET) == 0); // fseek returns 0 if succesful, SEEK_SET says to start looking from start of file
    ok = ok && (fread(bufferOut, g_BootSector.BytesPerSector, count, disk) == count); // fread returns the amount of succesfull elements read and placed into a variable
    return ok;
    // logical ands will make the function return a 0 if everything succeeds
    // This function is in reality a disguised void function

}

bool readFat(FILE* disk) {
    g_Fat = malloc(g_BootSector.SectorsPerFat * g_BootSector.BytesPerSector); // allocated memory which is used for a FAT
    return readSectors(disk, g_BootSector.ReservedSectors, g_BootSector.SectorsPerFat, g_Fat); // returns the read sectors which contain the FATs
}

bool readRootDirectory(FILE* disk) {
    uint32_t lba = g_BootSector.ReservedSectors + g_BootSector.FatCount * g_BootSector.SectorsPerFat;
    uint32_t size = sizeof(DirectoryEntry) * g_BootSector.DirectoryEntryCount;
    uint32_t sectors = size / g_BootSector.BytesPerSector;

    if (size % g_BootSector.BytesPerSector > 0) {
        sectors++;
    }

    g_RootDirectoryEnd = sectors + lba;
    g_RootDirectory = malloc(sectors * g_BootSector.BytesPerSector);
    return readSectors(disk, lba, sectors, g_RootDirectory);


}

DirectoryEntry* findFile(const char* name) {
    for (uint32_t i = 0; i < g_BootSector.DirectoryEntryCount; i++) {
        if (memcmp(name, g_RootDirectory[i].FileName, 11) == 0) { // compares the strings of the file name searched for and file nsmes in the root directory
            return &g_RootDirectory[i];                           // the last parameter is 11 as the names in Fat12 are allways 11 bytes long 8 for name 3 for extention
        }
    }
    return NULL;
}


bool readFile(FILE* disk, DirectoryEntry* fileEntry, uint8_t* buffer) {
    bool ok = true;
    uint16_t currentCluster = fileEntry->FirstClusterLow;
    printf("%i\n", currentCluster);
    uint32_t lba;
    uint32_t Fat_index;

    do {
        lba = g_RootDirectoryEnd + (currentCluster - 2) * g_BootSector.SectorsPerCluster;   // calculate the lba of the cluster pointed to in the root directory table
        ok = ok && readSectors(disk, lba, g_BootSector.SectorsPerCluster, buffer);
        buffer += g_BootSector.SectorsPerCluster * g_BootSector.BytesPerSector;

        Fat_index = currentCluster * 3 / 2;

        if (currentCluster % 2 == 0) {
            currentCluster = (*(uint16_t*)(g_Fat + Fat_index)) & 0x0fff;                   // explained here: https://wiki.osdev.org/FAT under section: File Allocation Table
        } else {
            currentCluster = (*(uint16_t*)(g_Fat + Fat_index)) >> 4;
        }
    } while (ok && currentCluster < 0xFF8);;
    return ok;
}


int main(int argc, char** argv) {


    argv[1] = "floppy.img";
    argv[2] = "TEXT    TXT";

    FILE* disk;

    disk = fopen(argv[1], "rb");                          // "rb" for read in binary mode

    if (!disk) {
        printf("Could not open disk image %s!!!\n", argv[1]);
        return -1;
    }
    

    if (!readBootSector(disk)) {
        printf("Could not read boot sector!!!\n");
        return -2;
    }
    

    if (!readFat(disk)) {
        printf("Could not read FAT!!!\n");
        free(g_Fat);
        return -3;
    }
    

    if (!readRootDirectory(disk)) {
        printf("Could not read root directory!!!\n");
        free(g_Fat);
        free(g_RootDirectory);
        return -4;
    }
    

    DirectoryEntry* fileEntry = findFile(argv[2]);

    if (!fileEntry) {
        printf("Could not find file!!!\n");
        free(g_Fat);
        free(g_RootDirectory);
        return -5;
    }    

    printf("%i\n", fileEntry->FileSize);

    // defines the memory buffer size
    uint32_t readSize = fileEntry->FileSize + g_BootSector.SectorsPerCluster * g_BootSector.BytesPerSector;
    
    printf("%i\n", readSize);


    uint8_t* buffer = malloc(readSize);



    if (!readFile(disk, fileEntry, buffer)) {
        printf("Could not read file!!!");
        free(g_Fat);
        free(buffer);
        free(g_RootDirectory);
        return -6;
    }


    for (int i = 0; i < fileEntry->FileSize; i++) {
        if (isprint(buffer[i]) || buffer[i] == 0x0a) {
            printf("%c", buffer[i]);
        } else {
            printf("<%02x>", buffer[i]);
        }
    }


    //printf("\n");
    //printf("%p\n", g_Fat);
    free(g_Fat);

    //printf("%p\n", g_RootDirectory);
    free(g_RootDirectory);

    //printf("%i\n", buffer);
    free(buffer);

    fclose(disk);
    return 0;
}