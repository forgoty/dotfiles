//Modify this file to change what commands output to your statusbar, and recompile using the make command.
static const Block blocks[] = {
	/*Icon*/	/*Command*/		/*Update Interval*/	/*Update Signal*/
	{"",	"sb-disk",	3600,	15},
	{"",	"sb-cpubars",	5,	19},
	{"",	"sb-cpu",	10,	16},
	{"",	"sb-memory",	10,	17},
	{"",	"sb-nettraf",	1,	18},
	{"",	"sb-volume",	0,	10},
	{"",	"sb-brightness",10,	2},
	{"",	"sb-battery",	5,	3},
	{"",	"sb-clock",	60,	1},
	{"",	"sb-internet",	5,	4},
};

//Sets delimiter between status commands. NULL character ('\0') means no delimiter.
static char *delim = " ";

// Have dwmblocks automatically recompile and run when you edit this file in
// vim with the following line in your vimrc/init.vim:

// autocmd BufWritePost ~/.local/src/dwmblocks/config.h !cd ~/.local/src/dwmblocks/; sudo make install && { killall -q dwmblocks;setsid dwmblocks & }

