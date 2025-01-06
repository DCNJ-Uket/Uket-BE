CREATE TABLE IF NOT EXISTS terms (
                                     id BIGINT AUTO_INCREMENT PRIMARY KEY,
                                     created_at DATETIME(6) NOT NULL,
                                     modified_at DATETIME(6) NOT NULL,
                                     document_no BIGINT NOT NULL,
                                     is_active BIT(1) NOT NULL,
                                     name VARCHAR(255) NOT NULL,
                                     type VARCHAR(255) NOT NULL
);

CREATE TABLE IF NOT EXISTS terms_sign (
                                          id BIGINT AUTO_INCREMENT PRIMARY KEY,
                                          created_at DATETIME(6) NOT NULL,
                                          modified_at DATETIME(6) NOT NULL,
                                          agreed_at DATETIME(6) NOT NULL,
                                          is_agreed BIT(1) NOT NULL,
                                          terms_id BIGINT NOT NULL,
                                          user_id BIGINT NOT NULL
);

CREATE TABLE IF NOT EXISTS document (
                                        id BIGINT AUTO_INCREMENT PRIMARY KEY,
                                        created_at DATETIME(6) NOT NULL,
                                        modified_at DATETIME(6) NOT NULL,
                                        document_no BIGINT NOT NULL,
                                        link VARCHAR(255) NOT NULL,
                                        name VARCHAR(255) NOT NULL,
                                        version BIGINT NOT NULL
);
