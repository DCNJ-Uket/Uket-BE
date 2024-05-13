package com.uket.domain.event.entity;

import static com.querydsl.core.types.PathMetadataFactory.*;

import com.querydsl.core.types.dsl.*;

import com.querydsl.core.types.PathMetadata;
import javax.annotation.processing.Generated;
import com.querydsl.core.types.Path;
import com.querydsl.core.types.dsl.PathInits;


/**
 * QShows is a Querydsl query type for Shows
 */
@Generated("com.querydsl.codegen.DefaultEntitySerializer")
public class QShows extends EntityPathBase<Shows> {

    private static final long serialVersionUID = 1770273421L;

    private static final PathInits INITS = PathInits.DIRECT2;

    public static final QShows shows = new QShows("shows");

    public final com.uket.domain.core.entity.QBaseEntity _super = new com.uket.domain.core.entity.QBaseEntity(this);

    //inherited
    public final DateTimePath<java.sql.Timestamp> createdAt = _super.createdAt;

    public final DateTimePath<java.time.LocalDateTime> endDate = createDateTime("endDate", java.time.LocalDateTime.class);

    public final QEvents event;

    public final NumberPath<Long> id = createNumber("id", Long.class);

    public final StringPath location = createString("location");

    //inherited
    public final DateTimePath<java.sql.Timestamp> modifiedAt = _super.modifiedAt;

    public final StringPath name = createString("name");

    public final DateTimePath<java.time.LocalDateTime> startDate = createDateTime("startDate", java.time.LocalDateTime.class);

    public final DateTimePath<java.time.LocalDateTime> ticketingDate = createDateTime("ticketingDate", java.time.LocalDateTime.class);

    public final NumberPath<Integer> totalTicketCount = createNumber("totalTicketCount", Integer.class);

    public QShows(String variable) {
        this(Shows.class, forVariable(variable), INITS);
    }

    public QShows(Path<? extends Shows> path) {
        this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
    }

    public QShows(PathMetadata metadata) {
        this(metadata, PathInits.getFor(metadata, INITS));
    }

    public QShows(PathMetadata metadata, PathInits inits) {
        this(Shows.class, metadata, inits);
    }

    public QShows(Class<? extends Shows> type, PathMetadata metadata, PathInits inits) {
        super(type, metadata, inits);
        this.event = inits.isInitialized("event") ? new QEvents(forProperty("event"), inits.get("event")) : null;
    }

}

