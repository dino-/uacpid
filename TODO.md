- Can some of the code in `main.hs` be factored into other
  modules? Maybe Uacpid.Signals to start with
- If the event handler script's shell process fails, we need to log
  the exit code. WARNING priority. The real question here is do we
  have any business waiting for the shell process to come back? Maybe
  just report on failure to launch?
- Need to be careful about when acpid is stopped and the socket
  goes away on a running uacpid. Must deal with this gracefully.
- If acpid has run but is not now running, the socket file
  in `/var/run/` can still be there. What happens then is that the
  connection is refused and you get no message in the uacpid log. You
  do get a message to stderr, but this is likely to be missed when
  running from `.xinitrc`  Need to fix this soon.
- Add version number output to logging when daemon is started
- Need to develop a simulator event server to use for automated
  testing and development
- Could use some unit tests (see simulator above also for other
  tests)
