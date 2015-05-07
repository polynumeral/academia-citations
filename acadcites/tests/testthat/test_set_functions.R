context('Manipulating string set representations.')


test_that('setParse returns expected output', {
    expect_equal(setParse(c('{1, 2, 3}', '{a, b}')),
                 data.frame(v1=c('1', 'a'),
                            v2=c('2', 'b'),
                            v3=c('3', NA), stringsAsFactors=FALSE)) })


test_that('setParse casts to types as expected.', {
    expect_equal(setParse(c('{1, 2, 3}', '{10, 100, 150}'), 'integer'),
                 data.frame(v1=c(1,10),
                            v2=c(2,100),
                            v3=c(3,150)))
})

test_that('setParse removes quotes from strings.', {
    expect_equal(setParse(c('{"Hello World", Quux}', '{Baz, "Foo Bar"}')),
                 data.frame(v1=c('Hello World', 'Baz'),
                            v2=c('Quux', 'Foo Bar'),
                            stringsAsFactors=FALSE))

    expect_equal(setParse(c('{\'Hello World\', Quux}', '{Baz, \'Foo Bar\'}')),
                 data.frame(v1=c('Hello World', 'Baz'),
                            v2=c('Quux', 'Foo Bar'),
                            stringsAsFactors=FALSE))
})

test_that('setParse ignores commas inside sets.', {
    expect_equal(setParse(c('{"Hello, World", Quux}', '{Baz, "Foo, Bar"}')),
                 data.frame(v1=c('Hello, World', 'Baz'),
                            v2=c('Quux', 'Foo, Bar'),
                            stringsAsFactors=FALSE))

    expect_equal(setParse(c('{\'Hello, World\', Quux}', '{Baz, \'Foo, Bar\'}')),
                 data.frame(v1=c('Hello, World', 'Baz'),
                            v2=c('Quux', 'Foo, Bar'),
                            stringsAsFactors=FALSE))
})

