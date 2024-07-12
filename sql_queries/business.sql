SELECT 
        business.j->>'business_id' AS business_id,
        business.j->>'postal_code' AS postal_code,
        (business.j->>'stars')::FLOAT AS stars,
        (business.j->>'review_count')::INT AS review_count,
        business.j->>'attributes' AS attributes
    FROM 
        public.business
    WHERE 
        business.j->>'categories' LIKE '%Restaurant%'
        AND (
        (business.j->'attributes'->>'RestaurantsPriceRange2') ~ '^\d+$'
        AND CAST((business.j->'attributes'->>'RestaurantsPriceRange2') AS INTEGER) = 2
    );