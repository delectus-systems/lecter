
// call after the dynamic shared library is loaded to initialize the
// Gambit runtime
extern int initDelectus (void);

// call before unloading the dynamic shared library to shut down the
// Gambit runtime
extern int finalizeDelectus (void);

