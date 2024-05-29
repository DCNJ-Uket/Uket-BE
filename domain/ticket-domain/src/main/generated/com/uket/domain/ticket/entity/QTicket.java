package com.uket.domain.ticket.entity;

import static com.querydsl.core.types.PathMetadataFactory.*;

import com.querydsl.core.types.dsl.*;

import com.querydsl.core.types.PathMetadata;
import javax.annotation.processing.Generated;
import com.querydsl.core.types.Path;
import com.querydsl.core.types.dsl.PathInits;


/**
 * QTicket is a Querydsl query type for Ticket
 */
@Generated("com.querydsl.codegen.DefaultEntitySerializer")
public class QTicket extends EntityPathBase<Ticket> {

    private static final long serialVersionUID = -1432141501L;

    private static final PathInits INITS = PathInits.DIRECT2;

    public static final QTicket ticket = new QTicket("ticket");

    public final com.uket.domain.core.entity.QBaseEntity _super = new com.uket.domain.core.entity.QBaseEntity(this);

    //inherited
    public final DateTimePath<java.sql.Timestamp> createdAt = _super.createdAt;

    public final com.uket.domain.event.entity.QEvents event;

    public final NumberPath<Long> id = createNumber("id", Long.class);

    //inherited
    public final DateTimePath<java.sql.Timestamp> modifiedAt = _super.modifiedAt;

    public final DateTimePath<java.time.LocalDateTime> paymentAt = createDateTime("paymentAt", java.time.LocalDateTime.class);

    public final com.uket.domain.event.entity.QReservation reservation;

    public final com.uket.domain.event.entity.QShows show;

    public final EnumPath<com.uket.domain.ticket.enums.TicketStatus> status = createEnum("status", com.uket.domain.ticket.enums.TicketStatus.class);

    public final com.uket.domain.user.entity.QUsers user;

    public QTicket(String variable) {
        this(Ticket.class, forVariable(variable), INITS);
    }

    public QTicket(Path<? extends Ticket> path) {
        this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
    }

    public QTicket(PathMetadata metadata) {
        this(metadata, PathInits.getFor(metadata, INITS));
    }

    public QTicket(PathMetadata metadata, PathInits inits) {
        this(Ticket.class, metadata, inits);
    }

    public QTicket(Class<? extends Ticket> type, PathMetadata metadata, PathInits inits) {
        super(type, metadata, inits);
        this.event = inits.isInitialized("event") ? new com.uket.domain.event.entity.QEvents(forProperty("event")) : null;
        this.reservation = inits.isInitialized("reservation") ? new com.uket.domain.event.entity.QReservation(forProperty("reservation"), inits.get("reservation")) : null;
        this.show = inits.isInitialized("show") ? new com.uket.domain.event.entity.QShows(forProperty("show"), inits.get("show")) : null;
        this.user = inits.isInitialized("user") ? new com.uket.domain.user.entity.QUsers(forProperty("user"), inits.get("user")) : null;
    }

}

