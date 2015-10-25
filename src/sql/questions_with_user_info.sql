SELECT question.created,vc_user.email,
       vc_user.given_name,vc_user.family_name,
       expression.surface,answer,
       question.session_id
        FROM question
 INNER JOIN session
         ON (session.ring_session = question.session_id)
 INNER JOIN vc_user
         ON (session.user_id = vc_user.id)
 INNER JOIN expression
         ON (question.source = expression.id)
      WHERE (1 = 0) -- below are some examples of WHERE conditionals you can use:
         OR (vc_user.email = 'isabelladepolo17@carondelet.net')
--         OR (vc_user.email = 'malenarico19@carondelet.net')
   ORDER BY question.created DESC;
