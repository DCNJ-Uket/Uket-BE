package com.uket.domain.core.entity;

import static com.querydsl.core.types.PathMetadataFactory.*;

import com.querydsl.core.types.dsl.*;

import com.querydsl.core.types.PathMetadata;
import javax.annotation.processing.Generated;
import com.querydsl.core.types.Path;


/**
 * QDeletableBaseEntity is a Querydsl query type for DeletableBaseEntity
 */
@Generated("com.querydsl.codegen.DefaultSupertypeSerializer")
public class QDeletableBaseEntity extends EntityPathBase<DeletableBaseEntity> {

    private static final long serialVersionUID = 72479422L;

    public static final QDeletableBaseEntity deletableBaseEntity = new QDeletableBaseEntity("deletableBaseEntity");

    public final QBaseEntity _super = new QBaseEntity(this);

    //inherited
    public final DateTimePath<java.sql.Timestamp> createdAt = _super.createdAt;

    public final DateTimePath<java.time.LocalDateTime> deletedAt = createDateTime("deletedAt", java.time.LocalDateTime.class);

    //inherited
    public final DateTimePath<java.sql.Timestamp> modifiedAt = _super.modifiedAt;

    public QDeletableBaseEntity(String variable) {
        super(DeletableBaseEntity.class, forVariable(variable));
    }

    public QDeletableBaseEntity(Path<? extends DeletableBaseEntity> path) {
        super(path.getType(), path.getMetadata());
    }

    public QDeletableBaseEntity(PathMetadata metadata) {
        super(DeletableBaseEntity.class, metadata);
    }

}

