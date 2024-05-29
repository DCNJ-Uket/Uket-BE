package com.uket.domain.user.entity;

import static com.querydsl.core.types.PathMetadataFactory.*;

import com.querydsl.core.types.dsl.*;

import com.querydsl.core.types.PathMetadata;
import javax.annotation.processing.Generated;
import com.querydsl.core.types.Path;
import com.querydsl.core.types.dsl.PathInits;


/**
 * QUsers is a Querydsl query type for Users
 */
@Generated("com.querydsl.codegen.DefaultEntitySerializer")
public class QUsers extends EntityPathBase<Users> {

    private static final long serialVersionUID = -682875118L;

    private static final PathInits INITS = PathInits.DIRECT2;

    public static final QUsers users = new QUsers("users");

    public final com.uket.domain.core.entity.QBaseEntity _super = new com.uket.domain.core.entity.QBaseEntity(this);

    //inherited
    public final DateTimePath<java.sql.Timestamp> createdAt = _super.createdAt;

    public final StringPath email = createString("email");

    public final NumberPath<Long> id = createNumber("id", Long.class);

    public final BooleanPath isRegistered = createBoolean("isRegistered");

    //inherited
    public final DateTimePath<java.sql.Timestamp> modifiedAt = _super.modifiedAt;

    public final StringPath name = createString("name");

    public final EnumPath<com.uket.domain.user.enums.Platform> platform = createEnum("platform", com.uket.domain.user.enums.Platform.class);

    public final StringPath platformId = createString("platformId");

    public final StringPath profileImage = createString("profileImage");

    public final EnumPath<com.uket.domain.user.enums.UserRole> role = createEnum("role", com.uket.domain.user.enums.UserRole.class);

    public final com.uket.domain.university.entity.QUniversity university;

    public final QUserDetails userDetails;

    public QUsers(String variable) {
        this(Users.class, forVariable(variable), INITS);
    }

    public QUsers(Path<? extends Users> path) {
        this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
    }

    public QUsers(PathMetadata metadata) {
        this(metadata, PathInits.getFor(metadata, INITS));
    }

    public QUsers(PathMetadata metadata, PathInits inits) {
        this(Users.class, metadata, inits);
    }

    public QUsers(Class<? extends Users> type, PathMetadata metadata, PathInits inits) {
        super(type, metadata, inits);
        this.university = inits.isInitialized("university") ? new com.uket.domain.university.entity.QUniversity(forProperty("university")) : null;
        this.userDetails = inits.isInitialized("userDetails") ? new QUserDetails(forProperty("userDetails")) : null;
    }

}

