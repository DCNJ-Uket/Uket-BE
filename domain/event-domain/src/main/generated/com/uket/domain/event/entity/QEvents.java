package com.uket.domain.event.entity;

import static com.querydsl.core.types.PathMetadataFactory.*;

import com.querydsl.core.types.dsl.*;

import com.querydsl.core.types.PathMetadata;
import javax.annotation.processing.Generated;
import com.querydsl.core.types.Path;
import com.querydsl.core.types.dsl.PathInits;


/**
 * QEvents is a Querydsl query type for Events
 */
@Generated("com.querydsl.codegen.DefaultEntitySerializer")
public class QEvents extends EntityPathBase<Events> {

    private static final long serialVersionUID = -1344284030L;

    private static final PathInits INITS = PathInits.DIRECT2;

    public static final QEvents events = new QEvents("events");

    public final com.uket.domain.core.entity.QBaseEntity _super = new com.uket.domain.core.entity.QBaseEntity(this);

    //inherited
    public final DateTimePath<java.sql.Timestamp> createdAt = _super.createdAt;

    public final DatePath<java.time.LocalDate> endDate = createDate("endDate", java.time.LocalDate.class);

    public final NumberPath<Long> id = createNumber("id", Long.class);

    public final StringPath location = createString("location");

    //inherited
    public final DateTimePath<java.sql.Timestamp> modifiedAt = _super.modifiedAt;

    public final StringPath name = createString("name");

    public final DatePath<java.time.LocalDate> startDate = createDate("startDate", java.time.LocalDate.class);

    public final com.uket.domain.university.entity.QUniversity university;

    public QEvents(String variable) {
        this(Events.class, forVariable(variable), INITS);
    }

    public QEvents(Path<? extends Events> path) {
        this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
    }

    public QEvents(PathMetadata metadata) {
        this(metadata, PathInits.getFor(metadata, INITS));
    }

    public QEvents(PathMetadata metadata, PathInits inits) {
        this(Events.class, metadata, inits);
    }

    public QEvents(Class<? extends Events> type, PathMetadata metadata, PathInits inits) {
        super(type, metadata, inits);
        this.university = inits.isInitialized("university") ? new com.uket.domain.university.entity.QUniversity(forProperty("university")) : null;
    }

}

