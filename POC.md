As PoC this project has failed since the *fsnotify* api does not react on *file
written* event, but only on *file changed* which is trigger to soon, and to
often.

The next PoC should use *inotify* directly, which is not cross platform but
looks more promising.  To be cross platform one could wrap calls to *inotify*
or *kqueue* manually.
