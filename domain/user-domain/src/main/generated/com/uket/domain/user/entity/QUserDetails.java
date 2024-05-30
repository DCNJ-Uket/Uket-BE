package com.uket.domain.user.entity;

import static com.querydsl.core.types.PathMetadataFactory.*;

import com.querydsl.core.types.dsl.*;

import com.querydsl.core.types.PathMetadata;
import javax.annotation.processing.Generated;
import com.querydsl.core.types.Path;


/**
 * QUserDetails is a Querydsl query type for UserDetails
 */
@Generated("com.querydsl.codegen.DefaultEntitySerializer")
public class QUserDetails extends EntityPathBase<UserDetails> {

    private static final long serialVersionUID = -165151359L;

    public static final QUserDetails userDetails = new QUserDetails("userDetails");

    public final StringPath depositorName = createString("depositorName");

    public final NumberPath<Long> id = createNumber("id", Long.class);

    public final StringPath phoneNumber = createString("phoneNumber");

    public final StringPath studentCode = createString("studentCode");

    public final StringPath studentMajor = createString("studentMajor");

    public final StringPath universityEmail = createString("universityEmail");

    public QUserDetails(String variable) {
        super(UserDetails.class, forVariable(variable));
    }

    public QUserDetails(Path<? extends UserDetails> path) {
        super(path.getType(), path.getMetadata());
    }

    public QUserDetails(PathMetadata metadata) {
        super(UserDetails.class, metadata);
    }

}

