-- TODO: Logical times maybe should be part of primary key. They also are structured objects so maybe a foreign key to something... (?)

CREATE TABLE IF NOT EXISTS Workspaces (
    id INTEGER PRIMARY KEY ASC,
    logicalTime INTEGER NOT NULL,
    parentWorkspaceId INTEGER NULL,
    question TEXT NOT NULL,
    FOREIGN KEY ( parentWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Messages (
    id INTEGER PRIMARY KEY ASC,
    logicalTimeSent INTEGER NOT NULL,
    sourceWorkspaceId INTEGER NOT NULL,
    targetWorkspaceId INTEGER NOT NULL,
    content TEXT NOT NULL,
    FOREIGN KEY ( sourceWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE
    FOREIGN KEY ( targetWorkspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Pointers (
    id INTEGER PRIMARY KEY ASC,
    content TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS Answers (
    workspaceId INTEGER PRIMARY KEY ASC, -- NOT NULL,
    logicalTimeAnswered INTEGER NOT NULL,
    answer TEXT NOT NULL,
    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS ExpandedPointers (
    workspaceId INTEGER NOT NULL,
    pointerId INTEGER NOT NULL,
    logicalTimeExpanded INTEGER NOT NULL,
    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE
    FOREIGN KEY ( pointerId ) REFERENCES Pointers ( id ) ON DELETE CASCADE
    PRIMARY KEY ( workspaceId ASC, pointerId ASC )
);

CREATE TABLE IF NOT EXISTS Commands (
    workspaceId INTEGER NOT NULL,
    localTime INTEGER NOT NULL,
    command TEXT NOT NULL,
    FOREIGN KEY ( workspaceId ) REFERENCES Workspaces ( id ) ON DELETE CASCADE
    PRIMARY KEY ( workspaceId ASC, localTime ASC )
);


-- RunQueue ?
