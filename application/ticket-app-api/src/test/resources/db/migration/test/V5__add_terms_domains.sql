CREATE TABLE IF NOT EXISTS terms (
                                     id BIGINT AUTO_INCREMENT PRIMARY KEY,
                                     created_at TIMESTAMP NOT NULL,
                                     modified_at TIMESTAMP NOT NULL,
                                     document_no BIGINT NOT NULL,
                                     is_active BOOLEAN NOT NULL, -- BIT(1) → BOOLEAN
                                     name VARCHAR(255) NOT NULL,
                                     type VARCHAR(255) NOT NULL
);

CREATE TABLE IF NOT EXISTS terms_sign (
                                          id BIGINT AUTO_INCREMENT PRIMARY KEY,
                                          created_at TIMESTAMP NOT NULL,
                                          modified_at TIMESTAMP NOT NULL,
                                          agreed_at TIMESTAMP NOT NULL,
                                          is_agreed BOOLEAN NOT NULL, -- BIT(1) → BOOLEAN
                                          terms_id BIGINT NOT NULL,
                                          user_id BIGINT NOT NULL
);

CREATE TABLE IF NOT EXISTS document (
                                        id BIGINT AUTO_INCREMENT PRIMARY KEY,
                                        created_at TIMESTAMP NOT NULL,
                                        modified_at TIMESTAMP NOT NULL,
                                        document_no BIGINT NOT NULL,
                                        link VARCHAR(255) NOT NULL,
                                        name VARCHAR(255) NOT NULL,
                                        version BIGINT NOT NULL
);
