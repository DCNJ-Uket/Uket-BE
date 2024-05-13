package com.uket.domain.event.entity;

import static com.querydsl.core.types.PathMetadataFactory.*;

import com.querydsl.core.types.dsl.*;

import com.querydsl.core.types.PathMetadata;
import javax.annotation.processing.Generated;
import com.querydsl.core.types.Path;
import com.querydsl.core.types.dsl.PathInits;


/**
 * QTicketing is a Querydsl query type for Ticketing
 */
@Generated("com.querydsl.codegen.DefaultEntitySerializer")
public class QTicketing extends EntityPathBase<Ticketing> {

    private static final long serialVersionUID = -1393826995L;

    private static final PathInits INITS = PathInits.DIRECT2;

    public static final QTicketing ticketing = new QTicketing("ticketing");

    public final com.uket.domain.core.entity.QBaseEntity _super = new com.uket.domain.core.entity.QBaseEntity(this);

    //inherited
    public final DateTimePath<java.sql.Timestamp> createdAt = _super.createdAt;

    public final DateTimePath<java.time.LocalDateTime> endTime = createDateTime("endTime", java.time.LocalDateTime.class);

    public final NumberPath<Long> id = createNumber("id", Long.class);

    //inherited
    public final DateTimePath<java.sql.Timestamp> modifiedAt = _super.modifiedAt;

    public final NumberPath<Integer> reservedCount = createNumber("reservedCount", Integer.class);

    public final QShows show;

    public final DateTimePath<java.time.LocalDateTime> startTime = createDateTime("startTime", java.time.LocalDateTime.class);

    public final NumberPath<Integer> totalCount = createNumber("totalCount", Integer.class);

    public final EnumPath<com.uket.domain.event.enums.TicketingUserType> type = createEnum("type", com.uket.domain.event.enums.TicketingUserType.class);

    public QTicketing(String variable) {
        this(Ticketing.class, forVariable(variable), INITS);
    }

    public QTicketing(Path<? extends Ticketing> path) {
        this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
    }

    public QTicketing(PathMetadata metadata) {
        this(metadata, PathInits.getFor(metadata, INITS));
    }

    public QTicketing(PathMetadata metadata, PathInits inits) {
        this(Ticketing.class, metadata, inits);
    }

    public QTicketing(Class<? extends Ticketing> type, PathMetadata metadata, PathInits inits) {
        super(type, metadata, inits);
        this.show = inits.isInitialized("show") ? new QShows(forProperty("show"), inits.get("show")) : null;
    }

}

