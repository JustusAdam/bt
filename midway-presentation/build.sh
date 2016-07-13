BUILD_DIR=_build
if [ ! -d $BUILD_DIR ];
then
    mkdir $BUILD_DIR;
fi
if [ ! -d $BUILD_DIR/images ];
then
    mkdir $BUILD_DIR/images;
fi

# cp images/* $BUILD_DIR/images

cp style.css $BUILD_DIR
pandoc -s -i -t revealjs \
    --slide-level=2 \
    -V slideNumber="\"c\"" \
    -V controls=false \
    -V theme=white \
    presentation.md \
    -o _build/index.html \
    -V margin=0.08 \
    --css style.css \
    --template=template.html
