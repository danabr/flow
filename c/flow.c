#define _BSD_SOURCE
#define _XOPEN_SOURCE

#include "sys/types.h"
#include "sys/stat.h"
#include "fcntl.h"
#include "stdlib.h"
#include "stdio.h"
#include "unistd.h"
#include "string.h"
#include "errno.h"
#include "error.h"
#include "time.h"

#define FLOW_FILE_PATH "/.flow"
#define LEDGER_FILE_PATH "/flow_ledger.csv"
#define ISO8601 "%Y-%m-%dT%H:%M:%S%z"

void start_flow(const char*);
void fail_flow(const char*);
void finish_flow(const char*, const char*);
void usage();
void write_or_fail(int, const char*, size_t);
const char* read_start_time();
const char* get_time();
int calculate_duration(const char*, const char*);
const char* format_duration(int);
time_t parse_time(const char*);
const char* get_flow_path();
const char* get_ledger_path();
const char* cat(const char*, const char*);
const char* get_home();
void* allocate();
int cmp(char*, char*);

int main(int argc, char* argv[]) {
  char* command = argv[1];
  const char* flow_path = get_flow_path();
  const char* ledger_path = get_ledger_path();
  if(command == NULL || cmp(command, "s") || cmp(command, "start")) {
    start_flow(flow_path);
  } else if(cmp(command, "d") || cmp(command, "done")) {
    finish_flow(flow_path, ledger_path);
  } else if(cmp(command, "f") || cmp(command, "fail")) {
    fail_flow(flow_path);
  }
  else {
    usage(argv[0]);
  }
  exit(0);
}

void start_flow(const char* flow_path) {
  int fd = open(flow_path, O_WRONLY | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);
  if(fd <= 0) {
    if(errno == EEXIST) {
      error(1, 0, "Previous flow started at %s already exists.", read_start_time(flow_path));
    } else {
      error(1, errno, "Failed to create flow file: %s.", flow_path);
    }
  }
  errno = 0;
  const char* start_time = get_time();
  int written = write(fd, start_time, strlen(start_time));
  if(written != (strlen(start_time))) {
    error(1, errno, "Failed to write %d bytes to flow file %s.", written, flow_path);
  }
}

void fail_flow(const char* flow_path) {
  if(unlink(flow_path) != 0 && errno != ENOENT) {
    error(1, errno, "Failed to remove flow file: %s.", flow_path);
  }
}

void finish_flow(const char* flow_path, const char* ledger_path) {
  const char* now = get_time();
  const char* start_time = read_start_time(flow_path);
  int duration = calculate_duration(start_time, now);
  const char* duration_str = format_duration(duration);
  int ledger_fd = open(ledger_path, O_WRONLY | O_APPEND | O_CREAT, S_IRUSR | S_IWUSR);
  if(ledger_fd <= 0) {
    error(1, errno, "Failed to store flow time to %s.", ledger_path);
  }
  char comma[] = {','};
  char newline[] = {'\n'};
  write_or_fail(ledger_fd, start_time, strlen(start_time));
  write_or_fail(ledger_fd, comma, 1);
  write_or_fail(ledger_fd, now, strlen(now));
  write_or_fail(ledger_fd, comma, 1);
  write_or_fail(ledger_fd, duration_str, strlen(duration_str));
  write_or_fail(ledger_fd, newline, 1);
  fail_flow(flow_path); // Remvove old flow file
  printf("Flow laster %ss.\n", duration_str);
}

void write_or_fail(int fd, const char* c, size_t size) {
  if(size != write(fd, c, size)) {
    error(1, errno, "Failed to store flow time. Write error.");
  }
}

int calculate_duration(const char* start_time, const char* end_time) {
  time_t start = parse_time(start_time);
  time_t end = parse_time(end_time);
  int duration = end - start;
  if(start == -1 || end == -1) {
    error(1, errno, "Failed to convert time.");
  }
  if(duration < 0) {
    error(1, 0, "Negative duration (%d). Time went backwards?", duration);
  }
  return duration;
}

time_t parse_time(const char* time_str) {
  struct tm* tm = allocate(sizeof(struct tm));
  if(strptime(time_str, ISO8601, tm) == NULL) {
    error(1, errno, "Failed to parse %s as time.", time_str);
  }
  return mktime(tm);
}

const char* format_duration(int duration) {
  char* duration_str = allocate(10);
  duration_str[9] = '\0';
  snprintf(duration_str, 9, "%d", duration);
  return duration_str;
}

const char* get_time() {
  time_t now = time(NULL);
  if(now == -1) { error(1, errno, "Failed to get system time."); }
  struct tm* local = localtime(&now);
  if(local == NULL) { error(1, errno, "Failed to get system time."); }
  char* buf = allocate(25);
  buf[24] = '\0';
  if(strftime(buf, 25, ISO8601, local) != 24) {
    error(1, 0, "Failed to format system time %s.", buf);
  }
  return buf;
}

const char* read_start_time(const char* flow_path) {
  int fd = open(flow_path, O_RDONLY);
  if(fd <= 0) {
    if(errno == ENOENT) {
      error(1, 0, "No flow started.");
    } else {
      error(1, errno, "Failed to open flow file %s.", flow_path);
    }
  }
  char* buf = allocate(30);
  buf[29] = '\0';
  ssize_t nread = read(fd, buf, 29); 
  if(nread == 0) {
    error(1, 0, "Invalid flow file %s.", flow_path);
  } else if(nread < 0) {
    error(1, errno, "Failed to read flow file %s.", flow_path);
  }
  return buf;
}

const char* get_flow_path() {
  return cat(get_home(), FLOW_FILE_PATH);
}

const char* get_ledger_path() {
  return cat(get_home(), LEDGER_FILE_PATH);
}

const char* get_home() {
  char* home = getenv("HOME");
  if(home == NULL) { error(1, 0, "HOME must be set."); }
  return home;
}

const char* cat(const char* a, const char* b) {
  char* buf = allocate(strlen(a) + 1 + strlen(b));
  strcat(buf, a);
  strcat(buf, b);
  return buf;
}

void* allocate(size_t size) {
  void* p = malloc(size);
  if(p == NULL) {
    error(1, errno, "Failed to allocate memory.");
  }
  return p;
}

void usage(char* progname) {
  printf("Usage:\n");
  printf("%s [start] - Start a new flow\n", progname);
  printf("%s d[one] - Record a succesful flow\n", progname);
  printf("%s f[ail] - Fail the current flow\n", progname);
  exit(1);
}

int cmp(char* a, char* b) {
  return strcmp(a, b) == 0;
}
