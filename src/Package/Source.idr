||| Types for specifying the source location (git, local, etc.) of a package.
module Package.Source

import Util


||| Whether we know if the source is pinned to a specific version.
public export
data PinKind : Type where
    ||| We know the source is pinned.
    IsPinned : PinKind

    ||| We are not sure whether the source is pinned or not.
    |||
    ||| This is useful when we read a source from a configuration file, as the
    ||| user can optionally supply a pin themselves.
    MaybePinned : PinKind


||| Specifies the version of a source.
||| @ pk The kind of pin
||| @ pinTy The type to store if the source is pinned.
public export
Pin : (pk : PinKind) -> (pinTy : Type) -> Type
Pin IsPinned    a = a
Pin MaybePinned a = Maybe a


||| The type used to identify a specific version of a git repository.
public export
CommitHash : Type
CommitHash = String


||| The location of the source files for a package.
public export
data Source : PinKind -> Type where
    ||| The source files are located on a repote git repository.
    Git : URL -> Pin sk CommitHash -> Source sk

    ||| The source files are located in a directory on the local mcahine.
    Local : FilePath -> Source sk

    ||| The source files are installed on the local machine using the legacy
    ||| "ipkg" system.
    Legacy : Source sk


||| Take a source which may or may not be pinned, and pin it. This involves
||| inferring a suitable version to use.
export
pinSource : Source MaybePinned -> M (Source IsPinned)
pinSource (Local fp)         = pure $ Local fp
pinSource Legacy             = pure $ Legacy
pinSource (Git url (Just x)) = pure $ Git url x
pinSource (Git url Nothing)  = Git url <$> gitRemoteLatestCommit url
